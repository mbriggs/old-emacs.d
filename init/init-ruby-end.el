(require 'ruby-end)
(setq ruby-end-insert-newline nil)

(define-key ruby-end-mode-map (kbd "RET") (lambda ()
                                            (interactive)
                                            (if (ruby-end-expand-p)
                                                (ruby-end-insert-end))
                                            (let ((ruby-end-mode nil))
                                              (call-interactively
                                               (key-binding
                                                (read-kbd-macro "RET"))))))
(provide 'init-ruby-end)
