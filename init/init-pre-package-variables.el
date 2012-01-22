(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(let ((colors (if (eq system-type 'darwin)
                  ["black" "#c60007" "#728a05" "#a57705" "#2075c7" "#c61b6e" "#259185" "white"]
                  ["black" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "white"])))
  (setq ansi-color-names-vector colors))

(provide 'init-pre-package-variables)
