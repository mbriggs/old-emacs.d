(require 'anything-config)
(require 'anything-match-plugin)

(setq anything-input-idle-delay 0.1)
(setq anything-idle-delay 0.1)

(defun anything-opened ()
  (interactive)
  (anything-other-buffer '(anything-c-source-buffers
                           anything-c-source-recentf)
                         "*anything opened*"))

(provide 'init-anything)