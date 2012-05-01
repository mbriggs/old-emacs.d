;;;; global

(define-key evil-normal-state-map "  " 'ace-jump-mode)
(define-key evil-normal-state-map " k" 'ace-jump-char-mode)
(define-key evil-normal-state-map " l" 'ace-jump-line-mode)
(define-key evil-normal-state-map " s" 'textmate-goto-symbol)
(define-key evil-normal-state-map " m" 'evil-jump-item)
(define-key evil-normal-state-map ",," 'evil-buffer)
(define-key evil-normal-state-map "-" 'delete-other-windows)
(define-key evil-normal-state-map "b" 'helm-opened)
(define-key evil-normal-state-map "E" 'ido-find-file)
(define-key evil-normal-state-map "\\" 'evil-repeat-find-char-reverse)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
(define-key evil-normal-state-map "L" 'evil-last-non-blank)
(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "<right>") 'next-error)
(define-key evil-normal-state-map (kbd "<left>") 'previous-error)
(define-key evil-normal-state-map (kbd "<C-return>") 'new-line-in-normal-mode)
(define-key evil-normal-state-map (kbd "M-t") 'textmate-goto-file)
(define-key evil-normal-state-map (kbd "M-f") 'dired)
(define-key evil-normal-state-map (kbd "C-w") 'delete-trailing-whitespace)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-next)
(define-key evil-normal-state-map (kbd "M-.") 'my-find-tag)
(define-key evil-normal-state-map (kbd "C-w") 'delete-trailing-whitespace)
(define-key evil-normal-state-map (kbd "C-SPC") 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map ",ve" 'dr/extract-variable)
(define-key evil-normal-state-map ",vi" 'dr/inline-variable)
(define-key evil-normal-state-map (kbd "M-k") 'cycle-buffer)
(define-key evil-normal-state-map (kbd "M-K") 'cycle-buffer-backward)

(define-key evil-normal-state-map (kbd "C-k") 'textmate-column-up)
(define-key evil-normal-state-map (kbd "C-j") 'textmate-column-down)
(define-key evil-normal-state-map (kbd "C-l") 'evil-forward-word-begin)
(define-key evil-normal-state-map (kbd "C-h") 'evil-backward-word-begin)

(global-set-key [f1] 'magit-status)
(global-set-key [f5] 'my-clear-all-caches)
(global-set-key [escape] 'keyboard-quit)
(global-set-key (kbd "M-a") 'mark-whole-buffer)
(global-set-key (kbd "C-\\") 'highlight-symbol-at-point)
(global-set-key (kbd "C-;") 'er/expand-region)
(global-set-key (kbd "C-:") 'er/contract-region)
(global-set-key (kbd "C-<backspace>") 'my-delete-backwards)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-]") 'textmate-shift-right)
(global-set-key (kbd "M-[") 'textmate-shift-left)
(global-set-key (kbd "M-j") 'other-window)
(global-set-key (kbd "M-.") 'my-find-tag)
(global-set-key (kbd "M-b") 'ibuffer)

(evil-ex-define-cmd "!" 'shell-command)
(evil-ex-define-cmd "history" 'magit-file-log)
(evil-ex-define-cmd "channel" 'ido-erc-buffer)
(evil-ex-define-cmd "semicolons" 'semi-colonize)
(evil-ex-define-cmd "create-spec" 'rtt/create-spec)
(evil-ex-define-cmd "align" 'align-regexp)
(evil-ex-define-cmd "[er]eval-region" 'eval-region)
(evil-ex-define-cmd "[eb]eval-buffer" 'eval-buffer)
(evil-ex-define-cmd "ack" 'ack)
(evil-ex-define-cmd "[al]ack-location" 'ack-location)
(evil-ex-define-cmd "[rc]run-clojure" 'clojure-jack-in)
(evil-ex-define-cmd "[rr]run-ruby" 'run-ruby)
(evil-ex-define-cmd "[rj]run-js" 'run-js)
(evil-ex-define-cmd "[re]run-elisp" 'ielm)
(evil-ex-define-cmd "[rh]run-haskell" 'run-haskell)
(evil-ex-define-cmd "serve-rails" 'serve-rails:start-project-server)
(evil-ex-define-cmd "erc" 'start-erc)
(evil-ex-define-cmd "weather" 'weather)
(evil-ex-define-cmd "rename-in-project" 'dr/rename-in-project)
(evil-ex-define-cmd "[sh]shell" 'shell)
(evil-ex-define-cmd "[de]debug-elisp" 'edebug-defun)
(evil-ex-define-cmd "dired" 'dired)
(evil-ex-define-cmd "twit" 'twit)
(evil-ex-define-cmd "create-spec" 'rtt/create-spec)
(evil-ex-define-cmd "[fb]find-blueprint" 'railgun-find-blueprint)
(evil-ex-define-cmd "[ff]find-factory" 'railgun-find-factory)
(evil-ex-define-cmd "[fs]find-schema" 'railgun-find-schema)



;;; esc quits

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;; helm

(define-key helm-map (kbd "M-n") 'helm-next-line)
(define-key helm-map (kbd "M-p") 'helm-previous-line)

;;; javascript

(evil-declare-key 'insert js3-mode-map (kbd "M-k") 'insert-js-function)
(evil-declare-key 'normal js3-mode-map ",t," 'rtt/toggle-test-and-implementation)
(evil-declare-key 'normal js3-mode-map ",g" 'add-to-js-globals)

;;; comint

(defun kill-comint ()
  (interactive)
  (comint-interrupt-subjob)
  (popwin:close-popup-window))

(evil-define-key 'normal comint-mode-map (kbd "q") 'kill-comint)


;;; magit

(define-key magit-branch-manager-mode-map (kbd "/") 'evil-search-forward)

;;; ruby

(evil-declare-key 'insert inf-ruby-mode-map
                  (kbd "M-k") 'require-absolute-ruby-path)

(evil-declare-key 'normal railway-minor-mode-map
                  ",m" 'railgun-find-model
                  ",c" 'railgun-find-controller
                  ",h" 'railgun-find-helper
                  ",v" 'railgun-find-view
                  ",p" 'railgun-find-presenter)

(evil-declare-key 'normal ruby-mode-map
                  ",tf" 'test-verify
                  ",ta" 'test-verify-all
                  ",t," 'rtt/toggle-test-and-implementation
                  ",tt" 'test-verify-single)

(evil-declare-key 'insert rhtml-mode-map
                  (kbd "M-=") 'insert-rhtml-%=
                  (kbd "M--") 'insert-rhtml-%
                  (kbd "M-k") 'insert-hashrocket)

(evil-declare-key 'insert ruby-mode-map
                  (kbd "M-k") 'insert-hashrocket)




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
  (kbd "C-o") 'evil-org-insert-heading
  (kbd "C-j") 'org-forward-same-level
  (kbd "C-k") 'org-backward-same-level)

(evil-define-key 'insert org-mode-map
  (kbd "C-=") 'org-todo
  (kbd "C-RET") 'evil-org-insert-heading)

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
            (define-key ido-completion-map (kbd "M-p") 'ido-prev-match)))

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
