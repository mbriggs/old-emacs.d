(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))


(setq el-get-sources
      '((:name textmate
               :type git
               :url "git://github.com/defunkt/textmate.el"
               :load "textmate.el")
        (:name css-mode :type elpa)
        (:name rvm
               :type git
               :url "http://github.com/djwhitt/rvm.el.git"
               :load "rvm.el"
               :compile ("rvm.el")
               :after (lambda() (rvm-use-default)))
        (:name fill-column-mode
               :type git
               :url "https://github.com/alpaker/Fill-Column-Indicator.git")
        (:name color-theme-sanityinc-solarized
               :type git
               :url "https://github.com/purcell/color-theme-sanityinc-solarized.git")
        (:name full-ack
               :type git
               :url "https://github.com/nschum/full-ack.git")
        (:name ack-and-a-half
               :type git
               :url "https://github.com/jhelwig/ack-and-a-half.git")
        (:name rhtml
               :type git
               :url "https://github.com/eschulte/rhtml.git"
               :features rhtml-mode)
        (:name yaml-mode 
               :type git
               :url "http://github.com/yoshiki/yaml-mode.git"
               :features yaml-mode)))

(setq my-packages
      (append
       '(ack-and-a-half
         auto-complete
         auto-complete-css
         auto-complete-etags
         auto-complete-ruby
         coffee-mode
         color-theme-solarized
         csv-mode
         enclose
         evil
         flymake-ruby
         haml-mode
         inf-ruby
         json
         linum-ex
         magit
         nxhtml
         mustache-mode
         textmate
         rainbow-delimiters
         rspec-mode
         ruby-block
         ruby-end
         rinari
         sass-mode
         scss-mode
         smex
         sunrise-commander
         sunrise-x-buttons
         textile-mode
         tidy
         rvm
         yaml-mode
         yasnippet)
       (mapcar 'el-get-source-name el-get-sources)))


(el-get 'sync my-packages)
