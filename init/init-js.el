(require 'js-comint)
(autoload 'js3-mode "js3" nil t)
;(require 'js2-highlight-vars)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.js.erb$" . js3-mode))
(add-to-list 'auto-mode-alist '("\\.json.erb$" . javscript-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(eval-after-load "coffee-mode"
  `(setq coffee-js-mode preferred-javascript-mode
         coffee-tab-width preferred-javascript-indent-level))

(add-hook 'coffee-mode-hook 'flymake-coffee-load)

;;; inf-js

(setq inferior-js-program-command "node-no-readline")
(defun add-inferior-js-keys ()
  ;; (local-set-key ",ce" 'js-send-last-sexp)
  ;; (local-set-key ",cb" 'js-send-buffer)
  (local-set-key ",ce" 'js-send-last-sexp-and-go)
  (local-set-key ",cb" 'js-send-buffer-and-go)
  (local-set-key ",cl" 'js-load-file-and-go))
;;(add-hook 'js-mode-hook 'add-inferior-js-keys)

;;; tern

;; (autoload 'tern-mode "tern.el" nil t)
;; (add-hook 'js3-mode-hook (lambda () (tern-mode t)))
;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))

;;; make functions pretty

(eval-after-load 'js3-mode
  '(font-lock-add-keywords
    'js3-mode `(("\\(function\\)("
                 (0 (progn (compose-region (match-beginning 1)
                                           (match-end 1) "λ")
                           nil))))))


(provide 'init-js)
