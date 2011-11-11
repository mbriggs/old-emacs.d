;(defvar solarized-colors
;  ;; name    sRGB      Gen RGB   degraded  ANSI(Solarized terminal)
;  '((base03  "#002b36" "#042028" "#1c1c1c" "#7f7f7f")
;    (base02  "#073642" "#0a2832" "#262626" "#000000")
;    (base01  "#586e75" "#465a61" "#4e4e4e" "#00ff00")
;    (base00  "#657b83" "#52676f" "#585858" "#ffff00")
;    (base0   "#839496" "#708183" "#808080" "#5c5cff")
;    (base1   "#93a1a1" "#81908f" "#8a8a8a" "#00ffff")
;    (base2   "#eee8d5" "#e9e2cb" "#d7d7af" "#e5e5e5")
;    (base3   "#fdf6e3" "#fcf4dc" "#ffffd7" "#ffffff")
;    (yellow  "#b58900" "#a57705" "#af8700" "#cdcd00")
;    (orange  "#cb4b16" "#bd3612" "#d75f00" "#ff0000")
;    (red     "#dc322f" "#c60007" "#af0000" "#cd0000")
;    (magenta "#d33682" "#c61b6e" "#af005f" "#cd00cd")
;    (violet  "#6c71c4" "#5859b7" "#5f5faf" "#ff00ff")
;    (blue    "#268bd2" "#2075c7" "#0087ff" "#0000ee")
;    (cyan    "#2aa198" "#259185" "#00afaf" "#00cdcd")
;    (green   "#859900" "#728a05" "#5f8700" "#00cd00"))



;; (defface solarized-string-delimiter-face
;;   '((t (:foreground "#c60007" :weight bold)))
;;    "string delimiters being the start and end sigil")

;; (mapchar (lambda (mode)
;;            (font-lock-add-keywords mode
;;                                    '(("\\s\"\\|\\s|" 0 'solarized-string-delimiter-face t))
;;                                    t)))


(add-hook 'after-change-major-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

(custom-set-faces
 '(anything-header ((((class color)) (:background "#81908F" :foreground "#fcf4dc"))))
 '(erb-face ((((class color) (min-colors 88) (background light)) (:background "#fcf4dc"))))
 '(flymake-errline ((((class color)) (:underline "#c60007"))))
 '(flymake-warnline ((((class color)) (:underline "#2075c7"))))
 '(isearch ((((class color)) (:background "#a57705" :foreground "#fcf4dc"))))
 '(js2-error-face ((((class color) (background light)) (:foreground "#c60007"))))
 '(js2-external-variable-face ((t (:foreground "#bd3612"))))
 '(js2-function-param-face ((t (:foreground "#728a05"))))
 '(lazy-highlight ((((class color)) (:background "#5859b7" :foreground "#fcf4dc"))))
 '(magit-branch ((((class color)) (:background "#81908F" :foreground "#FCF4DC"))))
 '(magit-diff-add ((((class color)) (:foreground "#728a05"))))
 '(magit-diff-del ((((class color)) (:foreground "#c60007"))))
 '(magit-section-title ((((class color)) (:background "#81908F" :foreground "#FCF4DC"))))
 '(rainbow-delimiters-depth-1-face ((((class color)) (:foreground "#c60007"))))
 '(rainbow-delimiters-depth-2-face ((((class color)) (:foreground "#a57705"))))
 '(rainbow-delimiters-depth-3-face ((((class color)) (:foreground "#bd3612"))))
 '(rainbow-delimiters-depth-4-face ((((class color)) (:foreground "#c61b6e"))))
 '(rainbow-delimiters-depth-5-face ((((class color)) (:foreground "#5859b7"))))
 '(rainbow-delimiters-depth-6-face ((((class color)) (:foreground "#2075c7"))))
 '(rainbow-delimiters-depth-7-face ((((class color)) (:foreground "#259185"))))
 '(rainbow-delimiters-depth-8-face ((((class color)) (:foreground "#728a05"))))
 '(rainbow-delimiters-depth-9-face ((((class color)) (:foreground "#042028"))))
 '(trailing-whitespace ((t (:background "#c7c0a9" :foreground "#e9e2cb" :inverse-video t)))))