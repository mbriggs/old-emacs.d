;;; railgun.el - be propelled to the right place by the power of magnets

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: navigation rails
;; Version: 1


;;; Commentary:

;; The goal of this project is to provide easy ways to get to the places you
;; want to be.

;; NOT DONE - railgun-find-views - show a list of views
;; NOT DONE - railgun-toggle-test - toggle between test and implementation (rspec or test/unit)
;; NOT DONE - railgun-create-test - create a test for a given thing
;; NOT DONE - railgun-create-spec - create a spec file for a given thing
;; NOT DONE - railgun-find-controller - jump to a given controller
;; - railgun-find-model - jump to a given model
;; - railgun-find-schema - find model entry in schema.rb file
;; - railgun-find-blueprint - find the entry in blueprints.rb for a given model (if you use machinist)
;; - railgun-start-server - start the server specified by the current project
;; - railgun-register-server - tell railgun how to register a new server

;;; servers

(defvar railgun/default-server 'thin)

(defvar railgun/servers
  '((thin      . ("rails" "server" "thin"))
    (passenger . ("passenger" "start"))
    (puma      . ("rails" "server" "puma"))
    (unicorn   . ("unicorn start"))))

(defun railgun-start-project-server ()
  (interactive)
  (let ((proj-server (eproject-attribute :rails-server)))
    (railgun-start-server proj-server)))

(defun railgun-start-server (&optional server)
  (interactive)
  (let ((current-dir default-directory))
    (cd (eproject-root))
    (let* ((server-name (or server railgun/default-server))
           (config (cdr (assoc server-name railgun/servers)))
           (command (car config))
           (args (cdr config))
           (out (apply 'make-comint "rails-server" command nil args)))
      (popwin:popup-buffer out :noselect t))
    (cd current-dir)))


;;;; Finders

(defun railgun-find-model ()
  (interactive)
  (find-file (railgun-file-name-for-model (railgun-prompt-for-resource))))

(defun railgun-find-controller ()
  (interactive)
  (let* ((model-location (railgun-file-name-for-model (railgun-prompt-for-resource)))
         (dir (replace-regexp-in-string "/models/" "/controllers" model-location))
         (controller (replace-regexp-in-string ".rb$" "_controller.rb" dir)))
    (find-file controller)))

;;;; jump to points in massive files

(defun railgun-find-blueprint ()
  (interactive)
  (let* ((root (eproject-root))
         (target (railgun-prompt-for-resource))
         (search (concat "^" target ".blueprint")))
    (find-file (concat root "test/blueprints.rb"))
    (or (re-search-forward search nil t)
        (re-search-backward search nil t))))

(defun railgun-find-schema ()
  (interactive)
  (let* ((name (railgun-table-name-for-model (railgun-prompt-for-resource)))
         (root (eproject-root))
         (regexp (concat "create_table \"" name "\"")))

    (find-file (concat root "db/schema.rb"))
    (or (re-search-forward regexp nil t)
        (re-search-backward regexp nil t))
    (message (concat "looking for " name))))

;;;; prompts

(defun railgun-prompt-for-resource ()
  (let* ((model (railgun-class-from-file-name (buffer-file-name)))
         (default (if (is-railgun-model-p) model)))
    (railgun-prompt "Model" (railgun-models) default)))

(defun railgun-prompt (prompt list & default)
  (let* ((prompt (concat prompt (if default (concat "(" default ")")) ": "))
         (value (ido-completing-read prompt list nil t)))
    (if (string= "" value) default value)))

;;;; Data

(defun railgun-model-files ()
  (all-files-under-dir-recursively (concat (eproject-root) "app/models") ".rb$"))

(defun railgun-controllers ()
  (mapcar 'railgun-controller-for-model (railgun-models)))

(defun railgun-models ()
  (mapcar 'car (railgun-models-alist)))

(defun railgun-table-name-for-model (model)
  (let* ((file (railgun-file-name-for-model model))
         (table (railgun-underscored-model-from-file-name file)))
    (pluralize-string table)))

(defun railgun-file-name-for-model (model)
  (cdr (assoc model (railgun-model-files-alist))))

(defun railgun-controller-for-model (model)
  (concat model "Controller"))


;;;; locations

(defvar railgun/locations
  '((models . "app/models/")
    (views . "app/views/")
    (controllers . "app/controllers/")
    (libs . "lib/")
    (configs . "config/")
    (unit-tests . "test/unit/")
    (functional-tests . "test/functional/")
    (specs . "spec/")))

(defun railgun-location (entity)
  (concat (eproject-root)
          (cdr (assoc entity (railgun/locations)))))

(defun railgun-file-name-from-path (entity path)
  (replace-regexp-in-string (railgun-location entity) "" path))

;;;; Caching

(defvar railgun/models-alist nil)
(defun railgun-models-alist ()
  (or railgun/models-alist
      (setq railgun/models-alist (mapcar 'railgun-class-and-file-name
                                         (railgun-model-files)))))

(defun railgun-clear-model-caches ()
  (interactive)
  (setq railgun/models-alist nil))

;;;; build cache

(defun railgun-class-and-file-name (file-name)
  (let ((class (railgun-class-from-file-name file-name)))
    `(,class . ,file-name)))

;;;; Predicates

(defun is-railgun-model-p ()
  (let ((model-regexp (concat "^" (eproject-root) "app/models")))
    (string-match model-regexp (buffer-file-name))))

;;;; Parsing

(defun railgun-underscored-model-from-file-name (file-name &optional ns)
  "get an underscored version of the current models name, passing in what to use as namespace delimiter"
  (let* ((delim-with (or ns "_"))
         (model-dir (concat (eproject-root) "app/models/"))
         (model (replace-regexp-in-string model-dir "" file-name))
         (filename (replace-regexp-in-string "/" delim-with model)))
    (replace-regexp-in-string ".rb$" "" filename)))

(defun railgun-class-from-file-name (file-name)
  (let* ((table-name (railgun-underscored-model-from-file-name file-name "::"))
         (capitalized (capitalize table-name)))
    (replace-regexp-in-string "_" "" capitalized)))



(provide 'railgun)
