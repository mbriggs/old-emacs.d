(provide 'my-ex-mode-maps)

(defun ex-mode-mapping (cmd)
  (let ((binding (car cmd))
        (fn (cdr cmd)))
    (evil-ex-define-cmd binding fn)))

(mapcar 'ex-mode-mapping
        '(("!"                        . shell-command)
          ("log"                      . magit-log)
          ("[br]branch"               . magit-branch-manager)
          ("deft"                     . deft)
          ("htmlize"                  . htmlize-region)
          ("[mm]minimap"              . toggle-minimap)
          ("reset-directory"          . reset-current-dir)
          ("history"                  . magit-file-log)
          ("bundle"                   . bundle-install)
          ("channel"                  . ido-erc-buffer)
          ("semicolons"               . semi-colonize)
          ("create-spec"              . railgun-create-spec)
          ("create-test"              . railgun-create-test)
          ("create-model"             . railgun-create-model)
          ("create-controller"        . railgun-create-controller)
          ("create-helper"            . railgun-create-helper)
          ("create-migration"         . railway-create-migration)
          ("create-javascript"        . railway-create-javascript)
          ("create-stylesheet"        . railway-create-stylesheet)
          ("align"                    . align-regexp)
          ("[er]eval-region"          . eval-region)
          ("[eb]eval-buffer"          . eval-buffer)
          ("ack"                      . ack)
          ("[al]ack-location"         . ack-location)
          ("[rc]run-clojure"          . clojure-jack-in)
          ("[rr]run-ruby"             . run-ruby)
          ("[rj]run-js"               . run-js)
          ("[re]run-elisp"            . ielm)
          ("[rh]run-haskell"          . run-haskell)
          ("[gl]gist-list"            . gist-list)
          ("[gr]gist-region"          . gist-region)
          ("[grp]gist-region-private" . gist-region-private)
          ("rserver"                  . serve-rails:start-project-server)
          ("jasmine"                  . serve-rails:start-jasmine)
          ("guard"                    . serve-rails:start-guard)
          ("spork"                    . serve-rails:start-spork)
          ("erc"                      . start-erc)
          ("weather"                  . weather)
          ("rename-in-project"        . dr/rename-in-project)
          ("[sh]shell"                . shell)
          ("[de]debug-elisp"          . edebug-defun)
          ("dired"                    . dired)
          ("twit"                     . twit)
          ("[fb]find-blueprint"       . railgun-find-blueprint)
          ("[ff]find-factory"         . railgun-find-factory)
          ("[fs]find-schema"          . railgun-find-schema)
          ("[wj]webjump"              . webjump)
          ("kill-hashes"              . ruby-onenine-ify-region-hashes)))

