;;;; global


(define-key evil-normal-state-map "  " 'ace-jump-mode)
(define-key evil-normal-state-map " k" 'ace-jump-char-mode)
(define-key evil-normal-state-map " l" 'ace-jump-line-mode)
(define-key evil-normal-state-map " s" 'textmate-goto-symbol)
(define-key evil-normal-state-map " m" 'evil-jump-item)
(define-key evil-normal-state-map ",," 'evil-buffer)
(define-key evil-normal-state-map "-" 'delete-other-windows)
(define-key evil-normal-state-map "b" 'ido-switch-buffer)
(define-key evil-normal-state-map "E" 'ido-find-file)
(define-key evil-normal-state-map "\\" 'evil-repeat-find-char-reverse)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
(define-key evil-normal-state-map "L" 'evil-last-non-blank)
(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "<C-return>") 'new-line-in-normal-mode)
(define-key evil-normal-state-map (kbd "M-t") 'textmate-goto-file)
(define-key evil-normal-state-map (kbd "M-f") 'dired)
(define-key evil-normal-state-map (kbd "C-w") 'delete-trailing-whitespace)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-next)
(define-key evil-normal-state-map (kbd "M-.") 'my-find-tag)
(define-key evil-normal-state-map (kbd "C-w") 'delete-trailing-whitespace)
(define-key evil-normal-state-map (kbd "C-SPC") 'comment-or-uncomment-region-or-line)
(define-key evil-normal-state-map (kbd "M-k") 'cycle-buffer)
(define-key evil-normal-state-map (kbd "M-K") 'cycle-buffer-backward)
(define-key evil-normal-state-map (kbd "M-o") 'session-jump-to-last-change)

(define-key evil-normal-state-map (kbd "C-k") 'textmate-column-up)
(define-key evil-normal-state-map (kbd "C-j") 'textmate-column-down)
(define-key evil-normal-state-map (kbd "C-l") 'evil-forward-word-begin)
(define-key evil-normal-state-map (kbd "C-h") 'evil-backward-word-begin)
(evil-define-key 'visual global-map (kbd ",re") 'dr/extract-variable)
(evil-define-key 'normal global-map (kbd ",ri") 'dr/inline-variable)

(global-set-key [f1] 'magit-status)
(global-set-key [f5] 'my-clear-all-caches)
(global-set-key [escape] 'keyboard-quit)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key (kbd "C-\\") 'highlight-symbol-at-point)
(global-set-key (kbd "M-;") 'er/expand-region)
(global-set-key (kbd "M-:") 'er/contract-region)
(global-set-key (kbd "C-<backspace>") 'my-delete-backwards)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-]") 'textmate-shift-right)
(global-set-key (kbd "M-[") 'textmate-shift-left)
(global-set-key (kbd "M-j") 'other-window)
(global-set-key (kbd "M-.") 'my-find-tag)
(global-set-key (kbd "M-b") 'ibuffer)
(global-set-key (kbd "M-v") 'evil-paste-after)
(global-set-key (kbd "M-RET") 'newline-anywhere)
(global-set-key (kbd "M-S-RET") 'newline-on-previous-line-anywhere)

(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-w") 'quit-window)
(global-set-key (kbd "M-w") 'quit-window)

;;; iedit

(global-set-key (kbd "M-L") 'iedit-mode)
(global-set-key (kbd "M-l") 'iedit-dwim)

;;; drop some keymaps

(require 'auto-complete)
(define-key *textmate-mode-map* [(meta return)] nil)
(define-key org-mode-map [(meta return)] nil)
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map [return] nil)
(define-key ruby-mode-map "{" nil)
(define-key ruby-mode-map "}" nil)


;;; esc quits

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;; helm

;; (define-key helm-map (kbd "M-n") 'helm-next-line)
;; (define-key helm-map (kbd "M-p") 'helm-previous-line)

;;; deft

(define-key deft-mode-map (kbd "M-n") 'next-line)
(define-key deft-mode-map (kbd "M-p") 'previous-line)
(define-key deft-mode-map (kbd "M-k") 'deft-delete-file)
(define-key deft-mode-map (kbd "M-r") 'deft-rename-file)

;;; javascript

;; (evil-declare-key 'insert js3-mode-map (kbd "M-k") 'insert-js-function)
(evil-declare-key 'normal js3-mode-map
                  ",g" 'add-to-js-globals
                  ",d" 'js-log-line
                  ",t," 'tjs-toggle-test-and-implementation
                  ",tc" 'tjs-create-test
                  ",ts" 'tjs-create-spec)

;;; comint

(defun kill-comint ()
  (interactive)
  (comint-interrupt-subjob)
  (popwin:close-popup-window))

(evil-define-key 'normal comint-mode-map (kbd "C-q") 'kill-comint)
(evil-define-key 'normal comint-mode-map (kbd "q") 'popwin:close-popup-window)


;;; magit

(define-key magit-branch-manager-mode-map (kbd "/") 'evil-search-forward)
(define-key magit-branch-manager-mode-map (kbd "C-n") 'evil-search-next)

;;; zencoding

(define-key zencoding-mode-keymap (kbd "M-e") 'zencoding-expand-line)

;;; ruby

;; (evil-declare-key 'insert inf-ruby-mode-map
;;                   (kbd "M-k") 'require-absolute-ruby-path)

(evil-declare-key 'normal railway-minor-mode-map
                  ",j" 'ra/find-javascript
                  ",s" 'ra/find-stylesheet
                  ",m" 'go-to-domain-or-model
                  ",c" 'railgun-find-controller
                  ",h" 'railgun-find-helper
                  ",v" 'railgun-find-view
                  ",l" 'railgun-find-lib
                  ",t," 'railgun-toggle-test-and-implementation
                  ",p" 'railgun-find-presenter)

(defun go-to-domain-or-model ()
  (interactive)
  (if (eproject-attribute :uses-domain)
      (setq railgun-entity 'domain)
    (setq railgun-entity 'model))

  (railgun-find-entity))

(evil-declare-key 'normal ruby-mode-map
                  ",d" 'ruby-debug-puts
                  ",tf" 'test-verify
                  ",ta" 'test-verify-all
                  ",tt" 'test-verify-single
                  ",hb" 'ruby-onenine-ify-buffer-hashes
                  ",hh" 'ruby-onenine-ify-line-hashes)

(evil-declare-key 'insert rhtml-mode-map
                  (kbd "M-=") 'insert-rhtml-%=
                  (kbd "M--") 'insert-rhtml-%)
                  ;(kbd "M-k") 'insert-hashrocket)

(evil-declare-key 'normal rhtml-mode-map
                  (kbd "M->") 'partially/visit-partial)

;; (evil-declare-key 'insert ruby-mode-map)
                  ;(kbd "M-k") 'insert-hashrocket)



;;; cucumber

(evil-declare-key 'normal feature-mode-map
                  ",tf" 'feature-verify-all-scenarios-in-buffer
                  ",tt" 'feature-verify-scenario-at-pos)

;;; clojure

(evil-declare-key 'normal clojure-mode-map
                  ",k" 'slime-compile-and-load-file
                  ",K" 'slime-repl-compile-and-load
                  ",d" 'slime-documentation
                  ",r" 'slime-repl-set-package
                  ",t" 'midje-check-fact
                  ",l" 'midje-recheck-last-fact-checked
                  ",n" 'midje-next-fact
                  ",u" 'midje-unfinished
                  ",s" 'midje-show-all-facts
                  ",h" 'midje-hide-all-facts)



;;; org

(evil-define-key 'normal org-mode-map
  (kbd "<tab>") 'org-cycle
  (kbd "M-L") 'org-metaright
  (kbd "M-H") 'org-metaleft
  (kbd "M-J") 'org-metadown
  (kbd "M-K") 'org-metaup
  (kbd "C-=") 'org-todo
  (kbd "M--") 'evil-org-normal-heading
  (kbd "C-j") 'org-forward-same-level
  (kbd "C-k") 'org-backward-same-level
  (kbd "M-_") 'evil-org-heading-after-current)

(evil-define-key 'insert org-mode-map
  (kbd "C-=") 'org-todo
  (kbd "M-l") 'org-metaright
  (kbd "M-h") 'org-metaleft
  (kbd "M-j") 'org-metadown
  (kbd "M-k") 'org-metaup
  (kbd "M--") 'evil-org-normal-heading
  (kbd "M-_") 'evil-org-heading-after-current)

;; ;;; fuzzy find

;; (fuzzy-find-initialize)
;; (define-key fuzzy-find-keymap "\M-n" 'fuzzy-find-next-completion)
;; (define-key fuzzy-find-keymap "\M-p" 'fuzzy-find-previous-completion)

;;; Magit

(evil-define-key 'normal magit-log-edit-mode-map "q" 'magit-log-edit-commit)

;;; Stuff I had some trouble defining normally

(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "M-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "M-p") 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'change-major-mode-hook
          (lambda ()
            (global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)))


(add-hook 'dired-mode-hook (lambda ()
  (define-key dired-mode-map "U" 'dired-up-directory)
  (define-key dired-mode-map "/" 'dired-isearch-filenames)))

(add-hook 'railway-minor-mode-hook 'evil-normalize-keymaps)

(provide 'my-keymaps)
