(require 'eproject)

(define-project-type ruby (generic)
  (look-for "Gemfile"))

(define-project-type emacs (generic)
  (look-for "init.el"))

(define-project-type vim (generic)
  (look-for "vimrc"))