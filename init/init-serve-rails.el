(add-hook 'serve-rails/starting-spork-hook 'set-spork-env)
(add-hook 'serve-rails/starting-guard-hook 'set-guard-env)
(add-hook 'serve-rails/launch-complete-hook 'clear-test-env-vars)

(defun set-spork-env ()
  (setenv "SPORK_RUNNING" "true")
  (setenv "NO_HYDRA" "true"))

(defun set-guard-env ()
  (setenv "GUARD_JASMINE" "true")
  (setenv "GUARD_MINITEST" "true")
  (setenv "GUARD_LIVERELOAD" "true"))

(defun clear-test-env-vars ()
  (interactive)
  (setenv "SPORK_RUNNING" "false")
  (setenv "NO_HYDRA" "false")
  (setenv "GUARD_JASMINE" "false")
  (setenv "GUARD_MINITEST" "false")
  (setenv "GUARD_LIVERELOAD" "false"))

(provide 'init-serve-rails)
