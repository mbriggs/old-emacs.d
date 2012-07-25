(defvar my-global-externs '("it" "loadFixtures" "expect" "describe" "beforeEach" "spyOn" "jasmine"
                            "$" "$j" "Mustache" "jQuery" "_" "qcloud" "Nulogy" "qc" "Backbone" "JST"
                            "afterEach" "setFixtures" "require" "Handlebars" "exports" "todo" "setTimeout"
                            "clearTimeout" "setInterval" "clearInterval"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-modes (quote (emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode java-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js2-mode js3-mode php-mode css-mode sass-mode scss-mode nxml-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode)))
 '(global-linum-mode t)
 '(js2-allow-keywords-as-property-names nil)
 '(js2-always-indent-assigned-expr-in-decls-p nil)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-enter-indents-newline t)
 '(js2-global-externs my-global-externs)
 '(js2-highlight-level 3)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode nil)
 '(js2-strict-missing-semi-warning nil)
 '(js3-global-externs my-global-externs)
 '(session-use-package t)
 '(linum-disabled-modes-list (quote (eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode erc-mode)))
 '(linum-format "%3d ")
 '(mumamo-chunk-coloring 1)
 '(rng-auto-validate-flag nil)
 '(scss-compile-at-save nil)
 '(tags-case-fold-search nil)
 '(zencoding-indentation 2)
 '(feature-cucumber-command "cucumber {options} {feature}")
 '(ruby-end-check-statement-modifiers nil)
 '(ruby-extra-keywords '("include" "extend" "private")))

  ;; name    sRGB      Gen RGB   degraded  ANSI(Solarized terminal)
  ;; '((base03  "#002b36" "#042028" "#1c1c1c" "#7f7f7f")
  ;;   (base02  "#073642" "#0a2832" "#262626" "#000000")
  ;;   (base01  "#586e75" "#465a61" "#4e4e4e" "#00ff00")
  ;;   (base00  "#657b83" "#52676f" "#585858" "#ffff00")
  ;;   (base0   "#839496" "#708183" "#808080" "#5c5cff")
  ;;   (base1   "#93a1a1" "#81908f" "#8a8a8a" "#00ffff")
  ;;   (base2   "#EAE2CB" "#e9e2cb" "#d7d7af" "#e5e5e5")
  ;;   (base3   "#fdf6e3" "#fcf4dc" "#ffffd7" "#ffffff")
  ;;   (yellow  "#b58900" "#a57705" "#af8700" "#cdcd00")
  ;;   (orange  "#cb4b16" "#bd3612" "#d75f00" "#ff0000")
  ;;   (red     "#dc322f" "#c60007" "#af0000" "#cd0000")
  ;;   (magenta "#d33682" "#c61b6e" "#af005f" "#cd00cd")
  ;;   (violet  "#6c71c4" "#5859b7" "#5f5faf" "#ff00ff")
  ;;   (blue    "#268bd2" "#2075c7" "#0087ff" "#0000ee")
  ;;   (cyan    "#2aa198" "#259185" "#00afaf" "#00cdcd")
  ;;   (green   "#859900" "#728a05" "#5f8700" "#00cd00"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 `(ruby-string-delimiter-face ((((class color)) (:foreground ,sol-red))))
 `(ruby-op-face ((((class color)) (:foreground ,sol-blue))
                 (t (:weight bold))))
 `(helm-selection ((((class color)) (:background ,sol-yellow :foreground ,sol-base3))))
 `(paren-face-match ((((class color)) (:background ,sol-orange :foreground ,sol-base3))))
 `(show-paren-match-face ((((class color)) (:background ,sol-yellow :foreground ,sol-base3))))
 `(helm-source-header ((((class color)) (:background ,sol-base00 :foreground ,sol-base3))))
 `(clojure-test-error-face ((((class color) (background light)) (:underline ,sol-yellow))))
 `(clojure-test-failure-face ((((class color) (background light)) (:underline ,sol-red))))
 `(clojure-test-success-face ((((class color) (background light)) (:background ,sol-green :foreground ,sol-base3))))
 `(erb-face ((((class color) (min-colors 88) (background light)) (:background ,sol-base3))))
 `(flymake-errline ((((class color)) (:underline ,sol-red))))
 `(flymake-warnline ((((class color)) (:underline ,sol-blue))))
 `(font-lock-function-name-face ((t (:foreground ,sol-base00 :weight bold))))
 `(hl-paren-face ((t (:weight bold))) t)
 `(isearch ((((class color)) (:background ,sol-yellow :underline ,sol-yellow :bold t :foreground ,sol-base3))))
 `(js2-error-face ((((class color) (background light)) (:foreground ,sol-red))))
 `(js2-external-variable-face ((t (:foreground ,sol-orange))))
 `(js2-function-param-face ((t (:foreground ,sol-green))))
 `(lazy-highlight ((((class color)) (:background ,sol-blue :foreground ,sol-base03 :bold t))))
 `(magit-branch ((((class color)) (:background ,sol-base1 :foreground ,sol-base3))))
 `(magit-diff-add ((((class color)) (:foreground ,sol-green))))
 `(magit-diff-del ((((class color)) (:foreground ,sol-red))))
 `(magit-section-title ((((class color)) (:background ,sol-base1 :foreground ,sol-base3))))
 `(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background light)) nil)))
 `(mumamo-border-face-in ((t (:inherit font-lock-preprocessor-face :weight bold))))
 `(mumamo-border-face-out ((t (:inherit font-lock-preprocessor-face :weight bold))))
 `(trailing-whitespace ((t (:background "#c7c0a9" :foreground ,sol-base2 :inverse-video t))))
 `(vhl/default-face ((t (:background ,sol-base2)))))
