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
        (:name rinari
               :type git
               :url "https://github.com/technomancy/rinari.git")
        (:name my-mooz-js2-mode
               :type git
               :url "https://github.com/mbriggs/js2-mode.git")
        (:name anything-project-files
               :type http
               :url "https://raw.github.com/lewang/anything-project-files/master/anything-project-files.el")
        (:name my-ruby-end
               :type http
               :url "https://raw.github.com/mbriggs/ruby-end/master/ruby-end.el"
               :features ruby-end)
        (:name anything-rails
               :type git
               :url "https://github.com/wolfmanjm/anything-on-rails.git"
               :features anything-rails)
        (:name lua-mode
               :type git
               :url "https://github.com/immerrr/lua-mode.git")
        (:name mo-git-blame
               :type git
               :url "https://github.com/mbunkus/mo-git-blame.git")
        (:name sass-mode
               :type git
               :url "https://github.com/nex3/sass-mode.git")
        (:name haml-mode
               :type git
               :url "https://github.com/nex3/haml-mode.git")
        (:name markdown-mode
               :type git
               :url "git://jblevins.org/git/markdown-mode.git")
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
         inf-ruby
         ruby-compilation
         anything
         anything-project-files
         anything-rails
         auto-complete
         auto-complete-css
         auto-complete-etags
         auto-complete-ruby
         coffee-mode
         color-theme-sanityinc-solarized
         color-theme-sanityinc
         color-theme-solarized
         color-theme-mac-classic
         color-theme-zen-and-art
         color-theme-tango-2
         csv-mode
         lua-mode
         evil
         flymake-ruby
         haml-mode
         json
         linum-ex
         magit
         markdown-mode
         mustache-mode
         my-ruby-end
         my-mooz-js2-mode
         textmate
         rainbow-delimiters
         rspec-mode
         rhtml-mode
         ruby-block
         sass-mode
         scss-mode
         smex
         textile-mode
         tidy
         rvm
         yaml-mode)
       (mapcar 'el-get-source-name el-get-sources)))


(el-get 'sync my-packages)
