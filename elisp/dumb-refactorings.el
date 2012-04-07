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

;; - dr/extract-variable   :: give the current region a name, create the variable right above it
;;                            and replace the region with the var name
;; - dr/inline-variable    :: take the current word the point is on, find what is on the other side
;;                            of the equals, delete the line, find the next instance of the variable,
;;                            and replace it with its definition
;; - dr/rename-in-project  :: do a regexp replace in multiple files for the given project
;;                            loosely based on mutli-replace by Bernard Vauquelin

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


;;; rename in proj

(defun dr/rename-in-project ()
  "Do the same query-replace on many files."
  (interactive)
  (let* ((default-search (current-word))
         (search-input (read-from-minibuffer (concat "Rename (" default-search "): ")))
         (search (if (string-equal "" search-input) default-search search-input))
         (replace (read-from-minibuffer (concat "Rename '" search "' to: ")))
         (search-root (ido-read-directory-name (concat "Rename '" search "' to '" replace "' in: ")
                                               (eproject-root)))
         (search-command (concat "fgrep -rl '" search "' " search-root))
         (oldbuf (current-buffer))
         (outbuf (get-buffer-create "*rename-in-proj-temp*")))

    (dr:create-rename-in-project-buffer outbuf)
    (when (dr:search-for-matches-in-project outbuf search-command)
      (beginning-of-buffer)
      (newline)

      (while (> (buffer-size) 1)
        (dr:report-files-left-to-go)
        (dr:query-replace-next-file name search replace)))
    (popwin:close-popup-window)
    (message "Done.")
    (switch-to-buffer oldbuf)))

(defun dr:search-for-matches-in-project (outbuf search-command)
  (zerop (call-process "/bin/sh" nil outbuf nil "-c" search-command)))

(defun dr:create-rename-in-project-buffer (outbuf)
  (popwin:popup-buffer outbuf :noselect t)
  (set-buffer outbuf)
  (erase-buffer))

(defun dr:report-files-left-to-go ()
  (beginning-of-buffer)
  (kill-line)
  (insert (concat (number-to-string (count-lines 2 (point-max))) " <------- files to go"))
  (next-line)
  (beginning-of-line))

(defun dr:query-replace-next-file (name search replace)
  (let ((name (buffer-substring (point) (progn (end-of-line) (point)))))
    (save-excursion
      (find-file name)
      (beginning-of-buffer)
      (query-replace search replace))
    (beginning-of-line)
        (kill-line 1)))

(provide 'dumb-refactorings)
