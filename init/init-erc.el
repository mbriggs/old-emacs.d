(defun growl (title message)
  (start-process "growl" " growl"
                 "/usr/local/bin/growlnotify"
                 title
                 "-a" "Emacs")
  (process-send-string " growl" message)
  (process-send-string " growl" "\n")
  (process-send-eof " growl"))

(defun osx-erc-mention (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (and (eq match-type 'current-nick)
       (not (erc-buffer-visible (current-buffer)))
       (growl
        (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
        message)))

(defun ido-erc-buffer()
  (interactive)
  (switch-to-buffer
   (ido-completing-read "Channel:"
                        (save-excursion
                          (delq nil
                                (mapcar (lambda (buf)
                                          (when (buffer-live-p buf)
                                            (with-current-buffer buf
                                              (and (eq major-mode 'erc-mode)
                                                   (buffer-name buf)))))
                                        (buffer-list)))))))

  (global-set-key (kbd "C-c e") 'rgr/ido-erc-buffer)

(if (eq system-type 'darwin)
    (add-hook 'erc-text-matched-hook 'osx-erc-mention))


(defun start-erc ()
  (interactive)

  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#clojure" "#emacs" "#meteor" "#javascript")))

  (setq erc-track-position-in-mode-line t)
  (setq erc-track-shorten-start 4)
  (setq erc-track-exclude '("irc.freenode.net"))

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
