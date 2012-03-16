(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-ndays 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-tags-column 80)


(setq org-default-notes-file "~/notes.org")
(define-key evil-normal-state-map (kbd ",r") 'org-capture)

; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc 
(setq org-outline-path-complete-in-steps t)

; keymaps
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-metaleft)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "C-=") 'org-todo)
(evil-define-key 'normal org-mode-map (kbd "C-o") 'evil-org-insert-heading)
(evil-define-key 'normal org-mode-map (kbd "C-j") 'org-forward-same-level)
(evil-define-key 'normal org-mode-map (kbd "C-k") 'org-backward-same-level)

(defun evil-org-insert-heading ()
  (interactive)
  (org-insert-heading-respect-content)
  (evil-insert-state))

(provide 'init-org)
