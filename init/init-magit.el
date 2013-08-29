(require 'magit)

;(setq magit-status-buffer-switch-function 'switch-to-buffer)
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
(add-hook 'magit-log-edit-mode-hook 'insert-ticket-number-from-branch-name)

(defun insert-ticket-number-from-branch-name ()
  (erase-buffer)
  (evil-insert-state)
  (let* ((current-branch (car (vc-git-branches)))
         (ticket-number (replace-regexp-in-string "[0-9]+\\(_.*\\)$" ""
                                                  current-branch nil nil 1)))
    (when (string-match "^[0-9]" current-branch)
      (insert (concat "#" ticket-number ": ")))))

; (defadvice magit-pop-to-log-edit (around magit-prevent-window-popup activate) (let ((pop-up-windows)) ad-do-it)) 
;(setq display-buffer-function 'display-buffer-same-window)
(defun magit-use-emacsclient-p ())

(provide 'init-magit)
