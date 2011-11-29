(require 'shoulda-mode)
(setq shoulda-use-rvm t)

(defun set-shoulda-command-to-proj-root ()
  (interactive)
  (let ((command (concat "(cd " (eproject-root) ";bundle exec ruby \"%f\" %o)")))
    (setq shoulda-command command)))

;;; For some reason, the gnu regex is capturing leading white space.
(setq compilation-error-regexp-alist
      (remq 'gnu compilation-error-regexp-alist))

(defadvice compile (around set-rail-test-dir (command &optional comint))
  "set the correct directory"
  (if (string-match "ruby" command)
      (let ((default-directory (eproject-root)))
        ad-do-it)
    ad-do-it))

(ad-activate 'compile)

(provide 'init-shoulda)
