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

(defun packman-eproject (name)
  (define-project-attribute `(,name . :project-name)
	   '(:use-shoulda t :packman t)))

(mapc 'packman-eproject
      '("d"
        "r"
        "dev"
        "rel"
        "packmanager"
        "production"))

(provide 'init-eproject)
