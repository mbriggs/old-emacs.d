(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

(define-key evil-normal-state-map (kbd "<f8>") 'git-blame-mode)

(provide 'init-git-blame)