(require 'webjump)

; hax - I want webjump to default to word-at-point if there is a word-at-point
(defun webjump-read-string (prompt)
  (let* ((prompt (concat prompt (if (word-at-point) (concat " (default: " (word-at-point) ")")) ": "))
         (input (read-string prompt)))
    (if (webjump-null-or-blank-string-p input) (word-at-point) input)))


(setq webjump-sites
      '(("Hacker News" . "http://news.ycombinator.com")

        ("Twitter" . "http://www.twitter.com")

        ("Underscore" . "http://underscorejs.org/")

        ("Backbone" . "http://backbonejs.org")

        ("MDN" .
         [simple-query "https://developer.mozilla.org"
                       "https://developer.mozilla.org/en-US/search?q=" ""])

        ("jQuery" .
         [simple-query "http://docs.jquery.com"
                       "http://docs.jquery.com/Special:Search?ns0=1&search=" ""])
        ("Google" .
         [simple-query "https://www.google.com/"
                       "https://www.google.com/search?q=" ""])

        ("APIDock - Ruby" .
         [simple-query "http://apidock.com"
                       "http://apidock.com/ruby/search?query=" ""])

        ("APIDock - Rails" .
         [simple-query "http://apidock.com"
                       "http://apidock.com/rails/search?query=" ""])))

(provide 'webjump)
