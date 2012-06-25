(add-hook 'serve-rails/starting-spork-hook 'set-spork-env)
(add-hook 'serve-rails/starting-guard-hook 'set-guard-env)

(defun set-spork-env ()
  (setenv "SPORK_RUNNING" "true")
  (setenv "NO_HYDRA" "true"))

(defun set-guard-env ()
  (setenv "GUARD_JASMINE" "true")
  (setenv "GUARD_MINITEST" "true")
  (setenv "NO_HYDRA" "true"))

(provide 'init-serve-rails)
