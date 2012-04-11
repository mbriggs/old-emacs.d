(add-hook 'magit-log-edit-mode-hook 'insert-ticket-number-from-branch-name)

(defun insert-ticket-number-from-branch-name ()
  (let* ((current-branch (car (vc-git-branches)))
         (ticket-number (replace-regexp-in-string "[0-9]+\\(_.*\\)$" ""
                                                  current-branch nil nil 1)))
    (if (string-match "^[0-9]" current-branch)
        (insert (concat "#" ticket-number ": ")))))

(provide 'init-magit)
