(autoload 'crontab-mode "crontab-mode" "Mode for editing crontab files" t)
(add-to-list 'auto-mode-alist '( "\\.?cron\\(tab\\)?\\'" . crontab-mode))

(provide 'init-crontab)
