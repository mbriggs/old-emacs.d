(require 'helm-config)
(require 'helm-match-plugin)
(require 'helm-buffers)
(require 'helm-files)

(setq helm-input-idle-delay 0.1)
(setq helm-idle-delay 0.1)

(defun helm-opened ()
  (interactive)
  (helm-other-buffer '(helm-c-source-buffers
                       helm-c-source-recentf)
                     "*helm opened*"))

(provide 'init-helm)
