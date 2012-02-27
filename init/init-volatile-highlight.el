(require 'volatile-highlights)

(vhl/give-advice-to-make-vhl-on-changes evil-paste-after)
(vhl/give-advice-to-make-vhl-on-changes evil-paste-before)
(vhl/give-advice-to-make-vhl-on-changes evil-paste-pop)

(volatile-highlights-mode t)

(provide 'init-volatile-highlight)
