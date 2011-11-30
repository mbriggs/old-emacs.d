(require 'midje-mode)
(add-hook 'midje-mode-hook
          (lambda ()
            (evil-define-key 'normal midje-mode-map
              ",mf" 'midje-check-fact
              ",ml" 'midje-recheck-last-fact-checked
              ",mk" 'midje-clear-comments
              ",mh" 'midje-hide-all-facts
              ",ms" 'midje-show-all-facts
              ",mn" 'midje-next-fact
              ",mp" 'midje-previous-fact
              ",mu" 'midje-unfinished)))

(add-hook 'clojure-mode-hook 'midje-mode)
(provide 'init-midje)