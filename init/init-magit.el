(require 'magit)

(add-hook 'magit-log-edit-mode-hook 'insert-ticket-number-from-branch-name)
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

(defun insert-ticket-number-from-branch-name ()
  (erase-buffer)
  (evil-insert-state)
  (let* ((current-branch (car (vc-git-branches)))
         (ticket-number (replace-regexp-in-string "[0-9]+\\(_.*\\)$" ""
                                                  current-branch nil nil 1)))
    (when (string-match "^[0-9]" current-branch)
      (insert (concat "#" ticket-number ": ")))))

(provide 'init-magit)
