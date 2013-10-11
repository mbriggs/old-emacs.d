(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

;;----------------------------------------------------------------------------
;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
;;----------------------------------------------------------------------------
(setq tab-always-indent 'complete)  ;; use 'complete when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)

;; hook AC into completion-at-point
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)


(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-yasnippet
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode
                js3-mode css-mode ruby-mode emacs-lisp-mode less-css-mode))
  (add-to-list 'ac-modes mode))


;; Exclude very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

;; (custom-set-variables
;;  '(ac-trigger-key "C-SPC")
;;  '(ac-auto-show-menu nil)
;;  '(ac-use-menu-map t))

;; (defun iy-ac-tab-noconflict ()
;;   (let ((command (key-binding [tab]))) ; remember command
;;     (local-unset-key [tab]) ; unset from (kbd "<tab>")
;;     (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")

;; (add-hook 'ruby-mode-hook 'iy-ac-tab-noconflict)
;; (add-hook 'markdown-mode-hook 'iy-ac-tab-noconflict)
;; (add-hook 'org-mode-hook 'iy-ac-tab-noconflict)


(provide 'init-autocomplete)
