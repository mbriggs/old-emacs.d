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

(define-project-attribute '("reporting" . :project-name)
  '(:uses-domain t))

(defun packman-eproject (name)
  (define-project-attribute `(,name . :project-name)
	   '(:packman t
       :uses-domain nil
       :rails-server passenger)))

(mapc 'packman-eproject
      '("d"
        "r"
        "dev"
        "icg"
        "rel"
        "packmanager"
        "production"))

(provide 'init-eproject)
