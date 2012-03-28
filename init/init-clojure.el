(require 'clojure-mode)
(require 'midje-mode)
(require 'ac-slime)

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(add-hook 'clojure-mode-hook 'midje-mode)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'clojure-mode-hook 'auto-complete-mode)

(provide 'init-clojure)
