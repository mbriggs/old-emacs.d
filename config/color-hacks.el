(defface solarized-string-delimiter-face
  '((t (:foreground "#c60007" :weight bold)))
   "string delimiters being the start and end sigil")

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (font-lock-add-keywords nil '(("\\s\"\\|\\s|" 0 'solarized-string-delimiter-face t)))
            (setq show-trailing-whitespace t)))

(custom-set-faces
 '(trailing-whitespace ((t (:background "#c7c0a9" :foreground "#e9e2cb" :inverse-video t))))
 '(lazy-highlight ((((class color)) (:background "#5859b7" :foreground "#fcf4dc"))))
 '(isearch ((((class color)) (:background "#a57705" :foreground "#fcf4dc" :inverse-video nil))))
 '(flymake-errline ((((class color)) (:underline "#c60007"))))
 '(flymake-warnline ((((class color)) (:underline "#2075c7"))))
 '(magit-branch ((((class color)) (:background "#81908F" :foreground "#FCF4DC"))))
 '(magit-section-title ((((class color)) (:background "#81908F" :foreground "#FCF4DC")))))