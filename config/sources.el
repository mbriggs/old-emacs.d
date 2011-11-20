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
        (:name ansi-color :type emacswiki)
        (:name linum-off :type emacswiki)
        (:name ruby-compilation :type elpa :features ruby-compilation)
        (:name eproject
               :type git
               :url "https://github.com/jrockway/eproject")
        (:name fuzzy-find-in-project
               :type git
               :url "https://github.com/justinweiss/fuzzy-find-in-project")
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
        (:name shoulda-mode
               :type http
               :url "https://raw.github.com/pezra/shoulda-mode/master/shoulda-mode.el")
        (:name evil-surround
               :type http
               :url "https://raw.github.com/timcharper/evil-surround/master/surround.el")
        (:name autopair
               :type http
               :url "http://autopair.googlecode.com/svn/trunk/autopair.el")
        (:name json
               :type http
               :url "https://raw.github.com/thorstadt/json.el/master/json.el")
        (:name escreen
               :type http
               :url "http://www.splode.com/~friedman/software/emacs-lisp/src/escreen.el")
        (:name lua-mode
               :type git
               :url "https://github.com/immerrr/lua-mode.git")
        (:name sass-mode
               :type git
               :url "https://github.com/nex3/sass-mode.git")
        (:name haml-mode
               :type git
               :url "https://github.com/nex3/haml-mode.git")
        (:name markdown-mode
               :type git
               :url "git://jblevins.org/git/markdown-mode.git")
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
       '(autopair
         ansi-color
         ack-and-a-half
         ruby-compilation
         inf-ruby
         anything
         auto-complete
         auto-complete-css
         auto-complete-etags
         auto-complete-ruby
         coffee-mode
         color-theme-solarized
         csv-mode
         lua-mode
         el-expectations
         evil
         evil-surround
         flymake-ruby
         haml-mode
         json
         linum-ex
         magit
         markdown-mode
         mode-compile
         mustache-mode
         ruby-end
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
         rvm
         yaml-mode)
       (mapcar 'el-get-source-name el-get-sources)))


(el-get 'sync my-packages)
