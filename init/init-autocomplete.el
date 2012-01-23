(require 'ac-dabbrev)
(substitute-key-definition 'ac-complete nil ac-completing-map)
;(ac-config-default)

(custom-set-variables '(ac-modes
                        '(emacs-lisp-mode
                          lisp-interaction-mode
                          c-mode
                          cc-mode
                          c++-mode
                          java-mode
                          perl-mode
                          cperl-mode
                          python-mode
                          ruby-mode
                          ecmascript-mode
                          javascript-mode
                          js2-mode
                          js3-mode
                          php-mode
                          css-mode
                          sass-mode
                          scss-mode
                          nxml-mode
                          makefile-mode
                          sh-mode
                          fortran-mode
                          f90-mode
                          ada-mode
                          xml-mode
                          sgml-mode)))

(global-auto-complete-mode t)
(setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(define-key ac-completing-map (kbd "C-n") 'dabbrev-expand)
(define-key ac-completing-map (kbd "C-p") 'dabbrev-expand)

;; Exclude very large buffers from dabbrev
(defun smp-dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'smp-dabbrev-friend-buffer)

(provide 'init-autocomplete)
