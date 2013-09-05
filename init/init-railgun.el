(defadvice rg-find-factory (around maybe-try-again activate)
  "If a factory can't be found, drop the first namespace and try again"
  (let ((file (rg-prompt-for-file "Factory for: " (rg-files))))
   (unless ad-do-it
     (when (eq 'domain (slot-value file 'type))
       (let ((file (clone file)))
         (set-slot-value file 'class (rg-remove "^.* ::" (slot-value file 'class)))
         (set-slot-value file 'relative-path (rg-remove "^.*/" (slot-value file 'relative-path)))
         ad-do-it)))))

(defadvice rg-find-schema (around maybe-try-again activate)
  "If a schema can't be found, drop the first namespace and try again"
  (let ((table-name (rg-prompt-for-table-name "Schema for: ")))
   (unless ad-do-it
     (let ((table-name (rg-remove "^.*_" table-name)))
       ad-do-it))))

(defun setup-railgun-for-qcloud ()
  (when (string= "qcloud" (projectile-project-name))
    (setq rg--class-paths
          '((model      . "app/models/")
            (controller . "app/controllers/")
            (presenter  . "app/presenters/")
            (repository . "app/repositories/")
            (helper     . "app/helpers/")
            (service    . "app/services/")
            (domain     . "domain/")
            (lib        . "lib/")
            (func-test  . "test/functional/")
            (spec       . "spec/")))))

(defun setup-railgun-for-packman ()
  (when (string= "packmanager" (projectile-project-name))
    (setq rg-factory-file-path "test/blueprints.rb")
    (setq rg--class-paths
          '((model      . "app/models/")
            (controller . "app/controllers/")
            (presenter  . "app/presenters/")
            (helper     . "app/helpers/")
            (service    . ("app/services/" . "app/services/[a-zA-Z-0-9_]+/"))
            (domain     . "domain/")
            (lib        . "lib/")
            (unit-test  . ("test/unit/" . "test/unit/\\(helper/\\)?"))
            (func-test  . "test/functional/")
            (spec       . ("spec/" . "spec/\\(domain/[a-zA-Z0-9_]+/\\|[a-zA-Z0-9_]+/\\)"))))))

(add-hook 'ruby-project-file-visit-hook 'setup-railgun-for-packman)
(add-hook 'ruby-project-file-visit-hook 'setup-railgun-for-qcloud)

(provide 'init-railgun)
