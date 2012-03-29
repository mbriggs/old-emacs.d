;;;; global

(define-key evil-normal-state-map ",," 'evil-buffer)
(define-key evil-normal-state-map "-" 'delete-other-windows)
(define-key evil-normal-state-map "E" 'ido-find-file)
(define-key evil-normal-state-map "  " 'evil-jump-item)
(define-key evil-normal-state-map "\\" 'evil-repeat-find-char-reverse)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "Y" 'copy-to-end-of-line)
(define-key evil-normal-state-map "L" 'evil-last-non-blank)
(define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "<right>") 'next-error)
(define-key evil-normal-state-map (kbd "<left>") 'previous-error)
(define-key evil-normal-state-map (kbd "<C-return>") 'new-line-in-normal-mode)
(define-key evil-normal-state-map (kbd "M-t") 'command-t)
(define-key evil-normal-state-map (kbd "M-s") 'textmate-goto-symbol)
(define-key evil-normal-state-map (kbd "M-b") 'helm-opened)
(define-key evil-normal-state-map (kbd "M-B") 'ibuffer)
(define-key evil-normal-state-map (kbd "M-f") 'dired)
(define-key evil-normal-state-map (kbd "M-.") 'find-tag)
(define-key evil-normal-state-map (kbd "C-w") 'delete-trailing-whitespace)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-next)
(define-key evil-normal-state-map (kbd "M-.") 'my-find-tag)
(define-key evil-normal-state-map (kbd "C-w") 'delete-trailing-whitespace)
(define-key evil-normal-state-map (kbd "C-SPC") 'comment-or-uncomment-region-or-line)
(define-key evil-visual-state-map ",ve" 'extract-variable)
(define-key evil-normal-state-map ",vi" 'inline-variable)
(define-key evil-normal-state-map (kbd "M-k") 'cycle-buffer)
(define-key evil-normal-state-map (kbd "M-K") 'cycle-buffer-backward)

(define-key evil-normal-state-map (kbd "C-j") 'evil-forward-paragraph)
(define-key evil-normal-state-map (kbd "C-k") 'evil-backward-paragraph)
(define-key evil-normal-state-map (kbd "C-l") 'evil-forward-word-begin)
(define-key evil-normal-state-map (kbd "C-h") 'evil-backward-word-begin)
(define-key evil-visual-state-map (kbd "C-j") 'evil-forward-paragraph)
(define-key evil-visual-state-map (kbd "C-k") 'evil-backward-paragraph)
(define-key evil-visual-state-map (kbd "C-l") 'evil-forward-word-begin)
(define-key evil-visual-state-map (kbd "C-h") 'evil-backward-word-begin)

(global-set-key (kbd "C-\\") 'highlight-symbol-at-point)
(global-set-key (kbd "C-;") 'er/expand-region)
(global-set-key (kbd "C-:") 'er/contract-region)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-]") 'textmate-shift-right)
(global-set-key (kbd "M-[") 'textmate-shift-left)
(global-set-key (kbd "M-j") 'other-window)
(global-set-key (kbd "M-.") 'my-find-tag)

;;; ruby

(evil-declare-key 'normal ruby-mode-map
                  ",tf" 'test-verify
                  ",ta" 'test-verify-all
                  ",t," 'test-toggle
                  ",tt" 'test-verify-single)

(evil-declare-key 'insert rhtml-mode-map
                  (kbd "M-=") 'insert-rhtml-%=
                  (kbd "M--") 'insert-rhtml-%)

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


(add-hook 'ido-minibuffer-setup-hook
          (lambda ()
            (define-key ido-completion-map "\C-n" 'ido-next-match)
            (define-key ido-completion-map "\C-p" 'ido-prev-match)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)
            (local-set-key [f1] 'yari)))

(add-hook 'change-major-mode-hook
          (lambda ()
            (global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)))


(add-hook 'dired-mode-hook (lambda ()
  (define-key dired-mode-map "U" 'dired-up-directory)
  (define-key dired-mode-map "/" 'dired-isearch-filenames)))

(evil-define-motion evil-jump-to-next-tag ()
  :jump t
  (let ((tag (thing-at-point 'symbol)))
    (find-tag tag t)))

(evil-define-motion evil-jump-to-previous-tag ()
  :jump t
  (let ((tag (thing-at-point 'symbol)))
    (find-tag tag ?-)))

(provide 'my-keymaps)
