(require 'shoulda-mode)
(setq shoulda-use-rvm t)

(defun set-shoulda-command-to-proj-root ()
  (interactive)
  (let ((command (concat "(cd " (eproject-root) ";bundle exec ruby \"%f\" %o)")))
    (setq shoulda-command command)))

(defadvice compile (around set-rail-test-dir (command &optional comint))
  "set the correct directory and don't pay attention to the gnu pattern"
  (if (string-match "ruby" command)
      (let ((default-directory (eproject-root))
            (compilation-error-regexp-alist (remq 'gnu compilation-error-regexp-alist)))
        ad-do-it)
    ad-do-it))

(ad-activate 'compile)

(provide 'init-shoulda)
