(autoload 'rhtml-mode "rhtml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode))

(evil-mode 1)
(evil-initial-state 'mo-git-blame 'emacs)
(evil-initial-state 'dired 'emacs)
(setq-default evil-shift-width 2)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require 'anything-project-files)

(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-samee "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(textmate-mode)
(substitute-key-definition 'ac-complete nil ac-completing-map)

(setq rspec-use-rake-flag nil)
(setq rspec-spec-command "rspec")

(require 'autopair)
(autopair-global-mode)

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
(setq anything-input-idle-delay 1)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)


(require 'tidy)
(require 'rinari)

(ruby-block-mode t)
(setq ruby-end-insert-newline nil)

(setq linum-format "%3d ")
(global-linum-mode)

(setq ibuffer-default-sorting-mode 'major-mode)

(setq show-trailing-whitespace t)
