(add-hook 'sldb-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))

(require 'autopair)
(autopair-global-mode)

(provide 'init-autopair)