(require 'surround)
(global-surround-mode 1)
(evil-mode 1)
(evil-initial-state 'mo-git-blame 'emacs)
(evil-initial-state 'dired 'emacs)
(setq-default evil-shift-width 2)

(provide 'init-evil)