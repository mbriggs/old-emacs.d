(require 'eproject)
                                        ;(require 'eproject-extras)

(define-project-type js (generic)
  (look-for "package.json"))

(define-project-type ruby (generic)
  (look-for "Gemfile"))

(define-project-type emacs (generic)
  (look-for "init.el"))

(define-project-type vim (generic)
  (look-for "vimrc"))

(define-project-type clojure (generic)
  (look-for "project.clj"))

(define-project-attribute '("reporting" . :project-name)
  '(:uses-domain t :rails-server passenger))

(defun packman-eproject (name)
  (define-project-attribute `(,name . :project-name)
    '(:packman t :uses-domain t :rails-server passenger)))

(mapc 'packman-eproject
      '("d"
        "r"
        "dev"
        "icg"
        "rel"
        "pm"
        "packmanager"
        "production"))

(defun set-packman-paths ()
  (setq railgun--class-paths
        '((model      . "app/models/")
          (controller . "app/controllers/")
          (presenter  . "app/presenters/")
          (helper     . "app/helpers/")
          (service    . ("app/services/" . "app/services/[a-zA-Z-0-9_]+/"))
          (js         . "app/assets/javascripts/")
          (domain     . ("domain/" . "domain/\\([a-zA-Z0-9_]+/\\)?"))
          (lib        . "lib/")
          (unit-test  . ("test/unit/" . "test/unit/\\(helper/\\)?"))
          (func-test  . "test/functional/")
          (spec       . ("spec/" . "spec/\\(domain/[a-zA-Z0-9_]+/\\|[a-zA-Z0-9_]+/\\)")))))

(provide 'init-eproject)
