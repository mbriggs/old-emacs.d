(require 'epg)

(twittering-icon-mode)
(setq-default twittering-timer-interval 60)
(setq-default twittering-url-show-status nil)
(setq-default twittering-use-master-password t)

(define-key twittering-mode-map "b" 'helm-opened)
(define-key twittering-mode-map ",," 'evil-buffer)

(provide 'init-twit)
