(autoload 'rhtml-mode "rhtml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode))

;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (ruby-mode "<%\\|<%=" "%>")
;;                   (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "erb"))
;; (multi-web-global-mode 1)

;; (require 'mumamo-fun)
;; (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))

;; (autoload 'mo-git-blame-file "mo-git-blame" nil t)
;; (autoload 'mo-git-blame-current "mo-git-blame" nil t)

(require 'surround)
(global-surround-mode 1)
(evil-mode 1)
(evil-initial-state 'mo-git-blame 'emacs)
(evil-initial-state 'dired 'emacs)
(setq-default evil-shift-width 2)


(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-samee "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(add-hook 'sldb-mode-hook
          #'(lambda () (setq autopair-dont-activate t)))

(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(add-hook 'highlight-parentheses-mode-hook
          '(lambda ()
             (setq autopair-handle-action-fns
                   (append
                    (if autopair-handle-action-fns
                        autopair-handle-action-fns
                      '(autopair-default-handle-action))
                    '((lambda (action pair pos-before)
                        (hl-paren-color-update)))))))


(require 'shoulda-mode)
(setq shoulda-use-rvm t)
(setq rspec-use-rake-flag nil)
(setq rspec-spec-command "rspec")

(require 'linum-off)

(require 'auto-complete-etags)
(substitute-key-definition 'ac-complete nil ac-completing-map)
(ac-config-default)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-to-list 'ac-sources 'ac-source-etags)

;; (enclose-global-mode t)
;; (enclose-add-encloser "|" "|")


;(defun my-ido-fuzzy-match (str items)
;  "Better ido fuzzy matching"
;
;(defvar my-ido-use-fuzzy-match t
;  "*Use my-ido-fuzzy-match for ido matching")
;
;(defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
;  "Choose between the regular ido-set-matches-1 and my-ido-fuzzy-match"
;  (if my-ido-use-fuzzy-match
;      (setq ad-return-value (my-ido-fuzzy-match ido-text (ad-get-arg 0)))
;    ad-do-it))
(setq anything-input-idle-delay 0.1)
(setq anything-idle-delay 0.1)
(textmate-mode)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(require 'autopair)
(autopair-global-mode)
(require 'fuzzy-find-in-project)

(require 'rinari)

;; (add-hook 'rinari-minor-mode-hook
;;           (lambda ()
;;             (fuzzy-find-project-root (rinari-root))))

(ruby-block-mode t)
(setq ruby-end-insert-newline nil)

(setq linum-format "%3d ")
(global-linum-mode)

(setq ibuffer-default-sorting-mode 'major-mode)

(setq show-trailing-whitespace t)
