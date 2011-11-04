(define-key evil-normal-state-map ",," 'evil-buffer)
(define-key evil-normal-state-map "\M-t" 'anything-for-files)
(define-key evil-normal-state-map "\M-j" 'evil-window-next)
(define-key evil-normal-state-map "E" 'ido-find-file)
(define-key evil-normal-state-map "  " 'evil-jump-item)
(define-key evil-normal-state-map "\\" 'evil-repeat-find-char-reverse)
(define-key evil-normal-state-map "H" 'evil-first-non-blank)
(define-key evil-normal-state-map "L" 'evil-last-non-blank)
(define-key evil-normal-state-map (kbd "<C-return>") 'new-line-in-normal-mode)
(define-key evil-normal-state-map "\M-b" 'anything-for-buffers)
(define-key evil-normal-state-map "\M-B" 'ibuffer)
(define-key evil-normal-state-map "\M-f" 'sunrise-cd)
(define-key evil-normal-state-map "\M-." 'find-tag)
(define-key evil-normal-state-map "\C-w" 'delete-trailing-whitespace)

(define-key evil-normal-state-map "\C-j" 'evil-forward-paragraph)
(define-key evil-normal-state-map "\C-k" 'evil-backward-paragraph)
(define-key evil-normal-state-map "\C-l" 'evil-forward-word-begin)
(define-key evil-normal-state-map "\C-h" 'evil-backward-word-begin)
(define-key evil-visual-state-map "\C-j" 'evil-forward-paragraph)
(define-key evil-visual-state-map "\C-k" 'evil-backward-paragraph)
(define-key evil-visual-state-map "\C-l" 'evil-forward-word-begin)
(define-key evil-visual-state-map "\C-h" 'evil-backward-word-begin)

(define-key evil-normal-state-map ",sc" 'rinari-find-controller)
(define-key evil-normal-state-map ",sh" 'rinari-find-helper)
(define-key evil-normal-state-map ",si" 'rinari-find-migration)
(define-key evil-normal-state-map ",sm" 'rinari-find-model)
(define-key evil-normal-state-map ",st" 'rinari-find-test)
(define-key evil-normal-state-map ",sv" 'rinari-find-view)
(define-key evil-normal-state-map ",sl" 'rinari-find-lib)

(setq yas/trigger-key nil)
(yas/reload-all)
(global-set-key (kbd "C-SPC") 'yas/expand)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-]") 'textmate-shift-right)
(global-set-key (kbd "M-[") 'textmate-shift-left)

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'change-major-mode-hook
          (lambda ()
            (global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)))
