(require 'autopair)
(autopair-global-mode)

(add-hook 'term-mode-hook
          (lambda ()
            (setq autopair-dont-activate t)))

(provide 'init-autopair)
