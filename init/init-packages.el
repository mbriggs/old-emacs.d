;;; Package.el

(require 'compile)
(require 'color-theme)
(defun require-package (package &optional min-version)
  "Ask elpa to install given PACKAGE."
  (unless (package-installed-p package min-version)
    (package-install package)))

;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24
(let ((package-el-site-lisp-dir (expand-file-name "~/.emacs.d/site-lisp/package")))
  (when (and (file-directory-p package-el-site-lisp-dir)
             (> emacs-major-version 23))
    (message "Removing local package.el from load-path to avoid shadowing bundled version")
    (setq load-path (remove package-el-site-lisp-dir load-path))))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))



;;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))


(setq el-get-sources
      '((:name enhanced-ruby-mode
               :type git
               :url "git://github.com/zenspider/enhanced-ruby-mode.git"
              :features ruby-mode)
        (:name haml-mode
               :type git
               :url "https://github.com/nex3/haml-mode")
        (:name ansi-color :type emacswiki)
        (:name etags-select :type emacswiki)
        (:name joseph-file-util :type emacswiki :features joseph-file-util)
        (:name gh
               :type git
               :url "https://github.com/sigma/gh.el.git")
        (:name cucumber
               :type git
               :url "git://github.com/michaelklishin/cucumber.el.git"
               :features feature-mode)
        (:name multiple-cursors
               :type git
               :url "https://github.com/magnars/multiple-cursors.el"
               :features multiple-cursors)
        (:name popwin
               :type git
               :url "https://github.com/m2ym/popwin-el.git"
               :features popwin)
        (:name buffer-tail
               :type git
               :url "https://github.com/mbriggs/buffer-tail.el.git"
               :features buffer-tail)
        (:name jade-mode
               :type git
               :url "https://github.com/ananthakumaran/jade-mode")
        (:name stylus-mode
               :type git
               :url "https://github.com/brianc/jade-mode.git")
        (:name expand-region
               :type git
               :url "https://github.com/mbriggs/expand-region.el.git")
        (:name deferred
               :type git
               :url "https://github.com/kiwanami/emacs-deferred")
        (:name find-file-in-project
               :type http
               :url "https://raw.github.com/ahobson/find-file-in-project/master/find-file-in-project.el"
               :features find-file-in-project)
        ;; (:name iedit-mode
        ;;        :type http
        ;;        :url "https://raw.github.com/eproxus/emacs-iedit/master/iedit.el"
        ;;        :features iedit)
        (:name pretty-mode
               :type http
               :url "https://raw.github.com/emacsmirror/pretty-mode/master/pretty-mode.el"
               :features pretty-mode)
        (:name htmlize
               :website "http://www.emacswiki.org/emacs/Htmlize"
               :description "Convert buffer text and decorations to HTML."
               :type http
               :url "http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi"
               :localname "htmlize.el"
               :feature htmlize)
        (:name toggle-friend-file
               :type http
               :website "https://github.com/gizmomogwai/toggle-friend-file"
               :description "Toggles between friend files (e.g. h and cpp). See customize-group toggle-friend-file."
               :url "https://github.com/gizmomogwai/toggle-friend-file/raw/master/toggle-friend-file.el"
               :features toggle-friend-file)
        (:name minimap
               :description "Minimap sidebar for Emacs"
               :type git
               :url "git://randomsample.de/minimap.git"
               :features minimap)
        (:name hl-tags-mode
               :website "http://www.reddit.com/r/emacs/comments/ha7l9/html_matching_tag_highlighting/"
               :description "if the cursor is inside of a '<div ....>' tag, it will highlight it and the corresponding '</div>' tag"
               :type git
               :url "https://github.com/deactivated/hl-tags-mode"
               :features hl-tags-mode)
        (:name hlinum
               :description "Extension for linum.el to highlight current line number"
               :type http :url "http://hlinum-mode.googlecode.com/hg/hlinum.el"
               :features hlinum
               :depends linum-ex)
        (:name popup
               :type git
               :url "https://github.com/auto-complete/popup-el")
        (:name auto-complete
               :type git
               :url "https://github.com/auto-complete/auto-complete")
        (:name auto-complete-emacs-lisp
               :description "Auto-complete sources for emacs lisp"
               :type http
               :url "http://www.cx4a.org/pub/auto-complete-emacs-lisp.el"
               :depends auto-complete)
        (:name auto-complete-ruby
               :description "Auto-complete sources for Ruby"
               :type http
               :url "http://www.cx4a.org/pub/auto-complete-ruby.el"
               :depends (auto-complete))
        (:name logito
               :type http
               :url "https://raw.github.com/sigma/logito/master/logito.el")
        (:name pcache
               :type http
               :url "https://raw.github.com/sigma/pcache/master/pcache.el")
        (:name cycle-buffer
               :type http
               :url "https://raw.github.com/emacsmirror/cycle-buffer/master/cycle-buffer.el"
               :features cycle-buffer)
        (:name dired-plus
               :type http
               :url "https://raw.github.com/emacsmirror/dired-plus/master/dired+.el")
        ;; (:name fuzzy-find-in-project
        ;;        :type git
        ;;        :url "https://github.com/justinweiss/fuzzy-find-in-project"
        ;;        :features fuzzy-find-in-project)
        (:name ido-hacks
               :type git
               :url "https://github.com/scottjad/ido-hacks"
               :features ido-hacks)
        ; (:name rvm
        ;        :type git
        ;        :url "http://github.com/djwhitt/rvm.el.git"
        ;        :load "rvm.el"
        ;        :compile ("rvm.el")
        ;        :after (progn (rvm-use-default)))
        (:name js3-mode
               :type git
               :url "https://github.com/thomblake/js3-mode.git")
        (:name serve-rails
               :type http
               :url "https://raw.github.com/mbriggs/serve-rails.el/master/serve-rails.el"
               :features serve-rails)
        (:name gist
               :type http
               :url "https://raw.github.com/defunkt/gist.el/master/gist.el")
        (:name session
               :type http
               :url "https://raw.github.com/emacsmirror/session/master/session.el"
               :features session)
        (:name dumb-refactorings
               :type http
               :url "https://raw.github.com/mbriggs/dumb-refactorings.el/master/dumb-refactorings.el"
               :features dumb-refactorings)
        (:name partially
               :type http
               :url "https://raw.github.com/mbriggs/partially.el/master/partially.el"
               :features partially)
        (:name less-mode
               :type http
               :url "https://raw.github.com/purcell/less-css-mode/master/less-css-mode.el"
               :features less-css-mode)
        (:name rails-assets
               :type http
               :url "https://raw.github.com/mbriggs/rails-assets/master/rails-assets.el"
               :features rails-assets)
        (:name railway
               :type http
               :url "https://raw.github.com/mbriggs/railway.el/master/railway.el"
               :features railway)
        (:name smart-forward
               :type http
               :url "https://raw.github.com/magnars/smart-forward.el/master/smart-forward.el"
               :features smart-forward)
        (:name railgun
               :type http
               :url "https://raw.github.com/mbriggs/railgun.el/master/railgun.el"
               :features railgun)
        (:name shoulda-test
               :type http
               :url "https://raw.github.com/mbriggs/shoulda-test/master/shoulda-test.el"
               :features shoulda-test)
        (:name solarized
               :type git
               :url "https://github.com/sellout/emacs-color-theme-solarized.git"
               :features color-theme-solarized)
               ;; :url "git@github.com:mbriggs/emacs-color-theme-solarized.git")
               ;; :url "https://github.com/bbatsov/solarized-emacs.git")
        (:name rspec-mode
               :type git
               :url "git@github.com:mbriggs/rspec-mode.git")
        ;; (:name my-haml-mode
        ;;        :type git
        ;;        :url "https://github.com/mbriggs/haml-mode"
        ;;        :features haml-mode)
        (:name weather
               :type http
               :url "https://raw.github.com/mbriggs/weather.el/master/weather.el"
               :features weather)
        (:name zossima
               :type git
               :url "https://github.com/technomancy/zossima"
               :features zossima)
        ;(:name web-mode
        ;       :type git
        ;       :url "https://github.com/mbriggs/web-mode"
        ;       ;; :url "https://github.com/fxbois/web-mode"
        ;       :features web-mode)
        (:name smartchr
               :type http
               :url "https://raw.github.com/imakado/emacs-smartchr/master/smartchr.el"
               :features smartchr)
        (:name volatile-highlights
               :type http
               :url "https://raw.github.com/k-talo/volatile-highlights.el/master/volatile-highlights.el")
        (:name evil-surround
               :type http
               :url "https://raw.github.com/timcharper/evil-surround/master/surround.el")
        (:name annoying-arrows
               :type http
               :url "https://raw.github.com/magnars/annoying-arrows-mode.el/master/annoying-arrows-mode.el")
        (:name lua-mode
               :type git
               :url "https://github.com/immerrr/lua-mode.git")
        (:name ac-dabbrev
               :type http
               :url "https://raw.github.com/emacsmirror/ac-dabbrev/master/ac-dabbrev.el")
        (:name ack-and-a-half
               :type git
               :url "https://github.com/jhelwig/ack-and-a-half.git")
        (:name rhtml
               :type git
               :url "https://github.com/eschulte/rhtml.git"
               :features rhtml-mode)))

;;; get what we can from elpa
(require-package 'anzu)
(require-package 'css-mode)
(require-package 'clojure-mode)
(require-package 'auto-complete)
(require-package 'ruby-compilation)
(require-package 'inf-ruby)
(require-package 'crontab-mode)
(require-package 'coffee-mode)
(require-package 'csv-mode)
(require-package 'diminish)
(require-package 'haskell-mode)
(require-package 'flymake-coffee)
(require-package 'flymake-ruby)
(require-package 'flymake-haml)
(require-package 'flymake-sass)
(require-package 'flymake-shell)
(require-package 'linum-off)
(require-package 'markdown-mode)
(require-package 'maxframe)
(require-package 'ac-nrepl)
(require-package 'move-text)
(require-package 'marmalade)
(require-package 'magit)
(require-package 'mic-paren)
(require-package 'mode-compile)
(require-package 'nrepl)
(require-package 'org)
(require-package 'paredit)
(require-package 'evil)
(require-package 'evil-paredit)
(require-package 'ruby-end)
(require-package 's)
(require-package 'projectile)
(require-package 'ruby-block)
(require-package 'rainbow-mode)
(require-package 'sass-mode)
(require-package 'scss-mode)
(require-package 'pymacs)
(require-package 'python-django)
(require-package 'scratch)
(require-package 'epc)
(require-package 'jedi)
(require-package 'yari)
(require-package 'yaml-mode)
(require-package 'yasnippet)
(require-package 'flx)
(require-package 'flx-ido)
(require-package 'flymake-python-pyflakes)



;;; el-get the rest
(setq my-packages
      (append
       '(enhanced-ruby-mode
         find-file-in-project
         autopair
         ace-jump-mode
         ansi-color
         ack-and-a-half
         auto-complete-css
         auto-complete-yasnippet
         auto-complete-etags
         auto-complete-emacs-lisp
         auto-complete-ruby
         ac-dabbrev
         cycle-buffer
         hlinum
         htmlize
         toggle-friend-file
         ;deft
         minimap
         hl-tags-mode
         dired-plus
         deferred
         diminish
         help+
         help-fns+
         dumb-refactorings
         lua-mode
         el-expectations
         emacs-w3m
         js-comint
         expand-region
         etags-select
         ido-hacks
         joseph-file-util
         js3-mode
         midje-mode
         haml-mode
         pretty-mode
         popwin
         textmate
         ;; flymake-node-jshint
         prolog-el
         gh
         pcache
         logito
         gist
         rhtml
         evil-surround
         ropemacs
         shoulda-test
         serve-rails
         railgun
         railway
         rails-assets
         session
         cucumber
         less-mode
         solarized
         jade-mode
         stylus-mode
         multiple-cursors
         smart-forward
         twittering-mode
         rspec-mode
         textile-mode
         volatile-highlights
         xml-parse
         multi-term
         python-mode
         highlight-symbol
         weather
         ;web-mode
         ;rvm
         zossima
         zencoding-mode)
       (mapcar 'el-get-source-name el-get-sources)))


(el-get 'sync my-packages)

(provide 'init-packages)
