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
(add-hook 'clojure-mode-hook 'auto-complete-mode)

 (require 'ac-nrepl)
 (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
 (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
 (eval-after-load "auto-complete"
   '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook 'auto-complete-mode)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
(setq nrepl-popup-stacktraces t)

(defun nrepl-eldoc-space (n)
  "Inserts a space and calls nrepl-eldoc to print arglists"
  (interactive "p")
  (self-insert-command n)
  (when (nrepl-current-session)
    (nrepl-eldoc)))

(defun my-clojure-mode-keys ()
  (define-key clojure-mode-map (kbd "SPC") 'nrepl-eldoc-space))

(add-hook 'clojure-mode-hook 'my-clojure-mode-keys)


(defun clojure-maybe-compile-and-load-file ()
  "Call function `nrepl-load-current-buffer' if there's an nrepl session.
   Meant to be used in `after-save-hook'."
  (when (and (eq major-mode 'clojure-mode)
             (not (string= "project.clj" buffer-file-name))
             (not (string-match "^.*\.cljs$" buffer-file-name))
             (nrepl-current-session))
    (nrepl-load-current-buffer)))
 
(add-hook 'after-save-hook 'clojure-maybe-compile-and-load-file)

(provide 'init-clojure)
