(require 'autopair)
(autopair-global-mode)

(add-hook 'term-mode-hook
          (lambda ()
            (setq autopair-dont-activate t)))

(set-default 'autopair-dont-activate #'(lambda () (memq major-mode '(sldb-mode
                                                                     clojure-mode
                                                                     nrepl-repl-mode))))

(provide 'init-autopair)
