(require 'eproject)
(require 'eproject-extras)

(define-project-type ruby (generic)
  (look-for "Gemfile"))

(define-project-type emacs (generic)
  (look-for "init.el"))

(define-project-type vim (generic)
  (look-for "vimrc"))

(define-project-type clojure (generic)
  (look-for "project.clj"))

(define-project-attribute '("dev" . :project-name)
  '(:use-shoulda t :packman t))

(define-project-attribute '("rel" . :project-name)
  '(:use-shoulda t :packman t))

(define-project-attribute '("packmanager" . :project-name)
  '(:use-shoulda t :packman t))

(provide 'init-eproject)
