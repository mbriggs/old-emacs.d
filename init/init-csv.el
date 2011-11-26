(autoload 'csv-mode "csv-mode" "Major mode for editing comma-separated value files." t)
(add-to-list 'auto-mode-alist '( "\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-nav-mode "csv-nav-mode" "Major mode for navigating comma-separated value files." t)

(setq csv-separators '("," ";" "|" " "))

(provide 'init-csv)
