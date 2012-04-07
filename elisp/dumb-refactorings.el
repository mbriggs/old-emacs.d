;;; dumb-refactorings.el - simple refactoring implemented as dumbly as possible

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: refactoring
;; Version: 1

;; dependancies:
;; - eproject
;; - popwin
;; - ido

;;; Commentary:

;; Some automated refactorings require a full AST tree of your entire project. Others can be
;; implemented in ways that are way more dumb, but still provide value.

;; bind the following functions to keys, or just call them via m-x

;; - dr/extract-variable   :: give the current region a name, create the variable right above it
;;                            and replace the region with the var name
;; - dr/inline-variable    :: take the current word the point is on, find what is on the other side
;;                            of the equals, delete the line, find the next instance of the variable,
;;                            and replace it with its definition
;; - dr/rename-in-project  :: do a regexp replace in multiple files for the given project


;;;; extract-variable

(defun dr/extract-variable ()
  "Create variable from region"
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


;;;; inline variable

(defun dr/inline-variable ()
  "Remove variable declaration at point, and replace its
   first usage with its definition"
  (interactive)
  (let ((name (current-word)))
    (re-search-forward "= ")
    (let ((value (buffer-substring (point) (point-at-eol))))
      (kill-whole-line)
      (search-forward name)
      (replace-match value))))

;; foo = 1.4 * whatever()
;; blah.blah(foo)


;;;; rename in proj

(defun dr/rename-in-project ()
  "Do the same query-replace on many files."
  (interactive)
  (let* ((search (dr:rename-string))
         (replace (read-from-minibuffer (concat "Rename '" search "' to: ")))
         (search-root (ido-read-directory-name (concat "Rename '" search "' to '" replace "' in: ")
                                               (eproject-root)))
         (oldbuf (current-buffer))
         (outbuf (dr:create-rename-in-project-buffer))
         (matched-files (dr:search-for-matches-in-project outbuf)))

    (while matched-files
      (dr:report-files-left-to-go search-root matched-files)
      (dr:query-replace-next-file (pop matched-files) search replace))
    (dr:clean-up oldbuf)))

;; get inputs

(defun dr:rename-string ()
  (let* ((default-search (current-word))
         (search-input (read-from-minibuffer (concat "Rename (" default-search "): "))))
    (if (string-equal "" search-input) default-search search-input)))

(defun dr:create-rename-in-project-buffer ()
  (let ((outbuf (get-buffer-create "*rename-in-proj*")))
    (popwin:popup-buffer outbuf :noselect t)
    (set-buffer outbuf)
    (erase-buffer)

    outbuf))

(defun dr:search-for-matches-in-project (outbuf)
  (zerop (call-process "/bin/sh" nil outbuf nil "-c"
                       (concat "fgrep -rl '" search "' " search-root)))
  (split-string (buffer-string) "\n" t))

;; perform rename

(defun dr:report-files-left-to-go (search-root matched-files)
  (beginning-of-buffer)
  (kill-line)
  (let ((current-file (replace-regexp-in-string search-root "" (car matched-files)))
        (files-left (number-to-string (- (length matched-files) 1))))
   (insert (concat "Currently On: " current-file ", " files-left " files to go")))
  (next-line)
  (beginning-of-line))


(defun dr:query-replace-next-file (file search replace)
  (save-excursion
    (find-file file)
    (beginning-of-buffer)
    (query-replace search replace))
  (beginning-of-line)
  (kill-line 1))

;; misc

(defun dr:clean-up (oldbuf)
  (popwin:close-popup-window)
  (message "Done.")
  (switch-to-buffer oldbuf))

(provide 'dumb-refactorings)
