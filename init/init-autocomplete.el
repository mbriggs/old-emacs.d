(require 'auto-complete-etags)
(substitute-key-definition 'ac-complete nil ac-completing-map)
(ac-config-default)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-to-list 'ac-sources 'ac-source-etags)

(provide 'init-autocomplete)