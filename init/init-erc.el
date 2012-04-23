(defun start-erc ()
  (interactive)

  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#clojure" "#emacs" "#ruby-lang"
                                       "#documentcloud" "#javascript"
                                       "#nulogy")))

  (setq erc-modules '(autoaway autojoin button completion fill irccontrols
                               list match menu move-to-prompt netsplit networks
                               noncommands readonly ring stamp spelling track))

  (setq erc-nick "mbriggs")
  (setq erc-prompt ">")
  (setq erc-user-full-name "Matt Briggs")

  ;; auto identify
  (require 'erc-services)
  (erc-services-mode 1)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords `((freenode ((erc-nick . ,erc-pass)))))
  (erc))

(provide 'init-erc)
