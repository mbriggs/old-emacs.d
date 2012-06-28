(require 'magit)

(add-hook 'magit-log-edit-mode-hook 'insert-ticket-number-from-branch-name)
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

(defun insert-ticket-number-from-branch-name ()
  (erase-buffer)
  (evil-insert-state)
  (let* ((current-branch (car (vc-git-branches)))
         (ticket-number (replace-regexp-in-string "[0-9]+\\(_.*\\)$" ""
                                                  current-branch nil nil 1)))
    (when (string-match "^[0-9]" current-branch)
      (insert (concat "#" ticket-number ": ")))))


;;; ==================> From Magit.next <=============================

;; (defun magit-filename (filename)
;;   "Return the path of FILENAME relative to its git repository.

;; If FILENAME is absolute, return a path relative to the git
;; repository containing it. Otherwise, return a path relative to
;; the current git repository."
;;   (let ((topdir (expand-file-name
;;                  (magit-get-top-dir (or (file-name-directory filename)
;;                                         default-directory))))
;;         (file (expand-file-name filename)))
;;     (when (and (not (string= topdir ""))
;;                ;; FILE must start with the git repository path
;;                (zerop (string-match-p (concat "\\`" topdir) file)))
;;       (substring file (length topdir)))))


;; ;; This variable is used to keep track of the current file in the
;; ;; *magit-log* buffer when this one is dedicated to showing the log of
;; ;; just 1 file.
;; (defvar magit-file-log-file nil)
;; (make-variable-buffer-local 'magit-file-log-file)

;; (defun magit-refresh-file-log-buffer (file range style)
;;   "Refresh the current file-log buffer by calling git.

;; FILE is the path of the file whose log must be displayed.

;; `magit-current-range' will be set to the value of RANGE.

;; STYLE controls the display. It is either `'long',  `'oneline', or something else.
;;  "
;;   (magit-configure-have-graph)
;;   (magit-configure-have-decorate)
;;   (magit-configure-have-abbrev)
;;   (setq magit-current-range range)
;;   (setq magit-file-log-file file)
;;   (magit-create-log-buffer-sections
;;     (apply #'magit-git-section nil
;;            (magit-rev-range-describe range (format "Commits for file %s" file))
;;            (apply-partially 'magit-wash-log style)
;;            `("log"
;;              ,(format "--max-count=%s" magit-log-cutoff-length)
;;              ,"--abbrev-commit"
;;              ,(format "--abbrev=%s" magit-sha1-abbrev-length)
;;              ,@(cond ((eq style 'long) (list "--stat" "-z"))
;;                      ((eq style 'oneline) (list "--pretty=oneline"))
;;                      (t nil))
;;              ,@(if magit-have-decorate (list "--decorate=full"))
;;              ,@(if magit-have-graph (list "--graph"))
;;              "--"
;;              ,file))))

;; (defun magit-file-log (&optional all)
;;   "Display the log for the currently visited file or another one.

;; With a prefix argument or if no file is currently visited, ask
;; for the file whose log must be displayed."
;;   (interactive "P")
;;   (let ((topdir (magit-get-top-dir default-directory))
;;         (current-file (magit-filename
;;                        (if (or current-prefix-arg (not buffer-file-name))
;;                            (magit-read-file-from-rev (magit-get-current-branch))
;;                         buffer-file-name)))
;;         (range "HEAD"))
;;     (magit-buffer-switch "*magit-log*")
;;     (magit-mode-init topdir 'magit-log-mode
;;                      #'magit-refresh-file-log-buffer
;;                      current-file range 'oneline)))

;; (defun magit-show-file-revision ()
;;   "Open a new buffer showing the current file in the revision at point."
;;   (interactive)
;;   (flet ((magit-show-file-from-diff (item)
;;                                     (switch-to-buffer-other-window
;;                                      (magit-show (cdr (magit-diff-item-range item))
;;                                                  (magit-diff-item-file item)))))
;;     (magit-section-action (item info "show")
;;       ((commit)
;;        (let ((current-file (or magit-file-log-file
;;                                (magit-read-file-from-rev info))))
;;          (switch-to-buffer-other-window
;;           (magit-show info current-file))))
;;       ((hunk) (magit-show-file-from-diff (magit-hunk-item-diff item)))
;;       ((diff) (magit-show-file-from-diff item)))))

;;; ==================> END Magit.next <=============================

(provide 'init-magit)
