(require 'diminish)
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode "ut"))

(eval-after-load "eproject"
  '(diminish 'eproject-mode "prj"))

(eval-after-load "volatile-highlights"
  '(diminish 'volatile-highlights-mode "vh"))

(eval-after-load "autopair"
  '(diminish 'autopair-mode "ap"))

(eval-after-load "rinari"
  '(diminish 'rinari-minor-mode "rin"))

(eval-after-load "auto-complete"
  '(diminish 'auto-complete-mode "ac"))


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "elisp")))

(add-hook 'js3-mode-hook
          (lambda ()
            (setq mode-name "js3")))

(provide 'init-diminish)
