(require 'clojure-mode)
(require 'midje-mode)

(add-hook 'clojure-mode-hook 'midje-mode)

(provide 'init-clojure)
