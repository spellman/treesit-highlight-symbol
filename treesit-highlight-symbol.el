;;; treesit-highlight-symbol.el --- Highlight symbol at point via tree-sitter -*- lexical-binding: t; -*-

;; Author: Cort Spellman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Minor mode that highlights all occurrences of the symbol under the
;; cursor within its enclosing scope, using tree-sitter for both symbol
;; identification and scope determination.
;;
;; Also usable as a library: `treesit-highlight-symbol-regions' returns
;; a list of (START . END) pairs without placing overlays.

;;; Code:

(require 'treesit)
(require 'seq)

;;;; Customization

(defgroup treesit-highlight-symbol nil
  "Highlight symbol at point using tree-sitter."
  :group 'treesit
  :prefix "treesit-highlight-symbol-")

(defface treesit-highlight-symbol-face
  '((t (:inherit highlight)))
  "Face for highlighted symbol occurrences."
  :group 'treesit-highlight-symbol)

(defcustom treesit-highlight-symbol-idle-delay 0.3
  "Seconds of idle time before highlighting."
  :type 'number
  :group 'treesit-highlight-symbol)

(defcustom treesit-highlight-symbol-scope-types
  '("function_definition" "method_definition" "class_definition"
    "module" "program")
  "Node types that define scope boundaries.
Used as a fallback when `treesit-defun-type-regexp' is not set
by the major mode."
  :type '(repeat string)
  :group 'treesit-highlight-symbol)

(defcustom treesit-highlight-symbol-ignored-node-types
  '("comment" "string" "string_content" "string_fragment"
    "template_string")
  "Node types to never highlight."
  :type '(repeat string)
  :group 'treesit-highlight-symbol)

;;;; Internal state

(defvar treesit-highlight-symbol--global-timer nil
  "Single idle timer shared by all buffers.")

(defvar treesit-highlight-symbol--buffers nil
  "Buffers with `treesit-highlight-symbol-mode' enabled.")

(defvar-local treesit-highlight-symbol--last-text nil)
(defvar-local treesit-highlight-symbol--last-start nil)

;;;; Core algorithm

(defun treesit-highlight-symbol--scope-predicate ()
  "Return a predicate matching scope boundary nodes.
Tests a node's type against `treesit-highlight-symbol-scope-types'.

`treesit-defun-type-regexp' is deliberately NOT consulted: major modes set
it for navigation, not lexical scoping, and some (notably `clojure-ts-mode')
make it match every sexp.  Treating that as a scope boundary collapses scope
to the innermost enclosing form, so only the symbol under point would be
highlighted.  When no ancestor matches `treesit-highlight-symbol-scope-types',
`treesit-highlight-symbol--find-scope' falls back to the buffer root node,
i.e. whole-file scope."
  (lambda (n)
    (member (treesit-node-type n)
            treesit-highlight-symbol-scope-types)))

(defun treesit-highlight-symbol--name-of-scope-p (node scope)
  "Return non-nil if NODE lies within the name child of SCOPE.
A function name lives inside its function_definition node structurally,
but is visible in the enclosing scope, not the function's own scope."
  (when-let* ((name-node (treesit-node-child-by-field-name scope "name")))
    (and (<= (treesit-node-start name-node) (treesit-node-start node))
         (>= (treesit-node-end name-node) (treesit-node-end node)))))

(defun treesit-highlight-symbol--find-scope (node)
  "Return the nearest scope ancestor of NODE, or the buffer root node.
When NODE is the name child of a scope boundary (e.g. a function name
inside its function_definition), skip that scope and use the enclosing
one, because the name is visible in the enclosing scope."
  (let* ((pred (treesit-highlight-symbol--scope-predicate))
         (scope (treesit-parent-until node pred)))
    (when (and scope
               (treesit-highlight-symbol--name-of-scope-p node scope))
      (setq scope (treesit-parent-until scope pred)))
    (or scope
        (treesit-buffer-root-node (treesit-node-language node)))))

(defun treesit-highlight-symbol--collect-matches (scope-node target-type target-text file-scope-p)
  "Find all nodes in SCOPE-NODE matching TARGET-TYPE and TARGET-TEXT.
When FILE-SCOPE-P is non-nil, restrict matches to the visible window.

Tree-sitter does the structural matching in C: the query captures every
node of TARGET-TYPE.  Text matching is then done in Lisp by comparing each
node's text to TARGET-TEXT.  The text comparison is not folded into the
query because Emacs' list-form query predicates (`:match', `:equal') fail
with \"Cannot find captured node\" when the predicate refers to its own
capture, across every grouping arrangement."
  (let* ((pattern `((,(intern target-type)) @match))
         (beg (when file-scope-p (window-start)))
         (end (when file-scope-p (window-end nil t)))
         (nodes (treesit-query-capture scope-node pattern beg end t)))
    (seq-filter (lambda (node)
                  (equal (treesit-node-text node t) target-text))
                nodes)))

(defun treesit-highlight-symbol--regions-for-node (node)
  "Return (START . END) pairs for all occurrences matching NODE."
  (let* ((target-text (treesit-node-text node t))
         (target-type (treesit-node-type node))
         (scope (treesit-highlight-symbol--find-scope node))
         (root (treesit-buffer-root-node (treesit-node-language node)))
         ;; treesit-node-eq compares the underlying C objects, so two
         ;; separately obtained root-node wrappers compare equal.
         (file-scope-p (treesit-node-eq scope root))
         (matches (treesit-highlight-symbol--collect-matches
                   scope target-type target-text file-scope-p)))
    (mapcar (lambda (n) (cons (treesit-node-start n) (treesit-node-end n)))
            matches)))

(defun treesit-highlight-symbol-regions ()
  "Return (START . END) pairs for all matching occurrences of symbol at point.
Returns nil if no suitable symbol at point or no tree-sitter parser."
  (when (treesit-parser-list)
    (let ((node (treesit-node-at (point))))
      (when (and node
                 (treesit-node-check node 'named)
                 (not (member (treesit-node-type node)
                              treesit-highlight-symbol-ignored-node-types)))
        (treesit-highlight-symbol--regions-for-node node)))))

;;;; Overlay management

(defun treesit-highlight-symbol--clear-overlays ()
  "Remove all highlight overlays in the current buffer."
  (remove-overlays (point-min) (point-max) 'treesit-highlight-symbol t))

(defun treesit-highlight-symbol--place-overlays (regions)
  "Place highlight overlays for REGIONS, a list of (START . END) pairs."
  (dolist (region regions)
    (let ((ov (make-overlay (car region) (cdr region))))
      (overlay-put ov 'treesit-highlight-symbol t)
      (overlay-put ov 'face 'treesit-highlight-symbol-face)
      (overlay-put ov 'priority 100)
      (overlay-put ov 'evaporate t))))

;;;; Idle timer callback

(defun treesit-highlight-symbol--highlight ()
  "Highlight all occurrences of the symbol at point."
  (let ((node (when (treesit-parser-list)
                (treesit-node-at (point)))))
    (if (and node
             (treesit-node-check node 'named)
             (not (member (treesit-node-type node)
                          treesit-highlight-symbol-ignored-node-types)))
        (let ((text (treesit-node-text node t))
              (start (treesit-node-start node)))
          (unless (and (equal text treesit-highlight-symbol--last-text)
                       (equal start treesit-highlight-symbol--last-start))
            (setq treesit-highlight-symbol--last-text text)
            (setq treesit-highlight-symbol--last-start start)
            (treesit-highlight-symbol--clear-overlays)
            (treesit-highlight-symbol--place-overlays
             (treesit-highlight-symbol--regions-for-node node))))
      (when treesit-highlight-symbol--last-text
        (treesit-highlight-symbol--clear-overlays)
        (setq treesit-highlight-symbol--last-text nil)
        (setq treesit-highlight-symbol--last-start nil)))))

(defun treesit-highlight-symbol--global-tick ()
  "Idle timer callback.  Highlight in the current buffer if the mode is active."
  (when treesit-highlight-symbol-mode
    (treesit-highlight-symbol--highlight)))

(defun treesit-highlight-symbol--on-scroll (_window _start)
  "Invalidate cache after scrolling so the next idle tick re-computes."
  (setq treesit-highlight-symbol--last-text nil))

(defun treesit-highlight-symbol--on-buffer-kill ()
  "Clean up when a buffer with the mode enabled is killed."
  (setq treesit-highlight-symbol--buffers
        (delq (current-buffer) treesit-highlight-symbol--buffers))
  (treesit-highlight-symbol--maybe-cancel-timer))

(defun treesit-highlight-symbol--ensure-timer ()
  "Start the global idle timer if not already running."
  (unless treesit-highlight-symbol--global-timer
    (setq treesit-highlight-symbol--global-timer
          (run-with-idle-timer
           treesit-highlight-symbol-idle-delay t
           #'treesit-highlight-symbol--global-tick))))

(defun treesit-highlight-symbol--maybe-cancel-timer ()
  "Cancel the global idle timer if no buffers use the mode."
  (when (and treesit-highlight-symbol--global-timer
             (null treesit-highlight-symbol--buffers))
    (cancel-timer treesit-highlight-symbol--global-timer)
    (setq treesit-highlight-symbol--global-timer nil)))

;;;; Minor mode

;;;###autoload
(define-minor-mode treesit-highlight-symbol-mode
  "Highlight occurrences of the symbol at point using tree-sitter."
  :lighter " TSHl"
  :group 'treesit-highlight-symbol
  (if treesit-highlight-symbol-mode
      (progn
        (setq treesit-highlight-symbol--last-text nil)
        (setq treesit-highlight-symbol--last-start nil)
        (unless (memq (current-buffer) treesit-highlight-symbol--buffers)
          (push (current-buffer) treesit-highlight-symbol--buffers))
        (treesit-highlight-symbol--ensure-timer)
        (add-hook 'kill-buffer-hook
                  #'treesit-highlight-symbol--on-buffer-kill nil t)
        (add-hook 'window-scroll-functions
                  #'treesit-highlight-symbol--on-scroll nil t))
    (setq treesit-highlight-symbol--buffers
          (delq (current-buffer) treesit-highlight-symbol--buffers))
    (treesit-highlight-symbol--maybe-cancel-timer)
    (treesit-highlight-symbol--clear-overlays)
    (setq treesit-highlight-symbol--last-text nil)
    (setq treesit-highlight-symbol--last-start nil)
    (remove-hook 'kill-buffer-hook
                 #'treesit-highlight-symbol--on-buffer-kill t)
    (remove-hook 'window-scroll-functions
                 #'treesit-highlight-symbol--on-scroll t)))

(provide 'treesit-highlight-symbol)
;;; treesit-highlight-symbol.el ends here
