(setq ansi-color-names-vector
      ["black" sol-red sol-green sol-yellow sol-blue sol-magenta sol-cyan "white"])

(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only)
  (linum-mode 0))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'init-ansi-color)
