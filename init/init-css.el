(autoload 'rainbow-turn-on "rainbow-mode" "Enable rainbow mode colour literal overlays")
(add-hook 'css-mode-hook 'rainbow-turn-on)
(add-hook 'html-mode-hook 'rainbow-turn-on)
(add-hook 'sass-mode-hook 'rainbow-turn-on)
(add-hook 'scss-mode-hook 'rainbow-turn-on)
(add-hook 'haml-mode-hook 'rainbow-turn-on)


(eval-after-load "auto-complete"
  '(progn
     (add-hook 'css-mode-hook 'ac-css-mode-setup)
     (add-hook 'sass-mode-hook 'ac-css-mode-setup)))

(provide 'init-css)
