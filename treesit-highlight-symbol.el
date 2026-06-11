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

(defvar-local treesit-highlight-symbol--timer nil)
(defvar-local treesit-highlight-symbol--last-text nil)
(defvar-local treesit-highlight-symbol--last-start nil)

;;;; Core algorithm

(defun treesit-highlight-symbol--scope-predicate ()
  "Return a predicate for scope boundary nodes.
Prefers `treesit-defun-type-regexp' (set by tree-sitter major modes),
falling back to `treesit-highlight-symbol-scope-types'."
  (if treesit-defun-type-regexp
      (if (consp treesit-defun-type-regexp)
          (car treesit-defun-type-regexp)
        treesit-defun-type-regexp)
    (lambda (n)
      (member (treesit-node-type n)
              treesit-highlight-symbol-scope-types))))

(defun treesit-highlight-symbol--find-scope (node)
  "Return the nearest scope ancestor of NODE, or the buffer root node."
  (let ((pred (treesit-highlight-symbol--scope-predicate)))
    (or (treesit-parent-until node pred)
        (treesit-buffer-root-node (treesit-node-language node)))))

(defun treesit-highlight-symbol--flatten-sparse-tree (tree)
  "Extract all matched nodes from a sparse TREE.
TREE is the structure returned by `treesit-induce-sparse-tree'."
  (let ((result nil))
    (when (car tree)
      (push (car tree) result))
    (dolist (child (cdr tree))
      (setq result (nconc (treesit-highlight-symbol--flatten-sparse-tree child)
                          result)))
    (nreverse result)))

(defun treesit-highlight-symbol--collect-matches (scope-node target-type target-text file-scope-p)
  "Find all nodes in SCOPE-NODE matching TARGET-TYPE and TARGET-TEXT.
When FILE-SCOPE-P is non-nil, restrict matches to the visible window."
  (let ((win-start (when file-scope-p (window-start)))
        (win-end (when file-scope-p (window-end nil t))))
    (let ((sparse-tree
           (treesit-induce-sparse-tree
            scope-node
            (lambda (n)
              (and (string= (treesit-node-type n) target-type)
                   (string= (treesit-node-text n t) target-text)
                   (or (not file-scope-p)
                       (let ((ns (treesit-node-start n)))
                         (and (>= ns win-start) (<= ns win-end)))))))))
      (when sparse-tree
        (treesit-highlight-symbol--flatten-sparse-tree sparse-tree)))))

(defun treesit-highlight-symbol-regions ()
  "Return (START . END) pairs for all matching occurrences of symbol at point.
Returns nil if no suitable symbol at point or no tree-sitter parser."
  (when (treesit-parser-list)
    (let ((node (treesit-node-at (point))))
      (when (and node
                 (treesit-node-check node 'named)
                 (not (member (treesit-node-type node)
                              treesit-highlight-symbol-ignored-node-types)))
        (let* ((target-text (treesit-node-text node t))
               (target-type (treesit-node-type node))
               (scope (treesit-highlight-symbol--find-scope node))
               (root (treesit-buffer-root-node (treesit-node-language node)))
               (file-scope-p (treesit-node-eq scope root))
               (matches (treesit-highlight-symbol--collect-matches
                         scope target-type target-text file-scope-p)))
          (mapcar (lambda (n)
                    (cons (treesit-node-start n) (treesit-node-end n)))
                  matches))))))

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
             (treesit-highlight-symbol-regions))))
      (treesit-highlight-symbol--clear-overlays)
      (setq treesit-highlight-symbol--last-text nil)
      (setq treesit-highlight-symbol--last-start nil))))

(defun treesit-highlight-symbol--tick (buf)
  "Idle timer callback.  Highlight in BUF if it is live and visible."
  (when (and (buffer-live-p buf)
             (get-buffer-window buf))
    (with-current-buffer buf
      (treesit-highlight-symbol--highlight))))

(defun treesit-highlight-symbol--on-scroll (_window _start)
  "Invalidate cache after scrolling so the next idle tick re-computes."
  (setq treesit-highlight-symbol--last-text nil))

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
        (setq treesit-highlight-symbol--timer
              (run-with-idle-timer
               treesit-highlight-symbol-idle-delay t
               #'treesit-highlight-symbol--tick (current-buffer)))
        (add-hook 'window-scroll-functions
                  #'treesit-highlight-symbol--on-scroll nil t))
    (when treesit-highlight-symbol--timer
      (cancel-timer treesit-highlight-symbol--timer)
      (setq treesit-highlight-symbol--timer nil))
    (treesit-highlight-symbol--clear-overlays)
    (setq treesit-highlight-symbol--last-text nil)
    (setq treesit-highlight-symbol--last-start nil)
    (remove-hook 'window-scroll-functions
                 #'treesit-highlight-symbol--on-scroll t)))

(provide 'treesit-highlight-symbol)
;;; treesit-highlight-symbol.el ends here
