;;; dumb-refactorings.el - simple refactoring implemented as dumbly as possible

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: refactoring
;; Version: 1


;;; Commentary:

;; Some automated refactorings require a full AST tree of your entire project. Others can be
;; implemented in ways that are way more dumb, but still provide value.

;; - extract-variable :: give the current region a name, create the variable right above it
;;                       and replace the region with the var name
;; - inline-variable  :: take the current word the point is on, find what is on the other side
;;                       of the equals, delete the line, find the next instance of the variable,
;;                       and replace it with its definition

(defun dr/extract-variable ()
  (interactive)
  (let ((start (region-beginning))
        (end (region-end))
        (name (read-from-minibuffer "Variable name: ")))
    (kill-region start end)
    (insert name)
    (beginning-of-line)
    (newline)
    (previous-line)
    (indent-for-tab-command)
    (insert (concat name " = "))
    (yank)))

;; foo = DoSomething(1 + 1)

(defun dr/inline-variable ()
  (interactive)
  (let ((name (current-word)))
    (re-search-forward "= ")
    (let ((value (buffer-substring (point) (point-at-eol))))
      (kill-whole-line)
      (search-forward name)
      (replace-match value))))

;; foo = 1.4 * whatever()
;; blah.blah(foo)

(provide 'dumb-refactorings)
