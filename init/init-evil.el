(require 'evil)
(require 'surround)

(global-surround-mode 1)
(evil-mode 1)
(setq-default evil-shift-width 2)

(defun set-mode-to-default-emacs (mode)
  (evil-set-initial-state mode 'emacs))

(mapcar 'set-mode-to-default-emacs
        '(dired
          magit-branch-manager-mode
          magit-commit-mode
          magit-log-mode
          log-view-mode
          deft-mode))

(provide 'init-evil)
