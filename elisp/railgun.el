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


;; finders

(defun railgun-find-blueprint ()
  (interactive)
  (let* ((root (eproject-root))
         (target (railgun-prompt-for-model))
         (search (concat "^" target ".blueprint")))
    (find-file (concat root "test/blueprints.rb"))
    (or (re-search-forward search nil t)
        (re-search-backward search nil t))))

(defun railgun-find-schema ()
  (interactive)
  (let* ((name (railgun-table-name-for-model (railgun-prompt-for-model)))
         (root (eproject-root))
         (regexp (concat "create_table \"" name "\"")))

    (find-file (concat root "db/schema.rb"))
    (or (re-search-forward regexp nil t)
        (re-search-backward regexp nil t))
    (message (concat "looking for " name))))

(defun railgun-find-model ()
  (interactive)
  (find-file (railgun-file-name-for-model (railgun-prompt-for-model))))

(defun railgun-find-controller ()
  (interactive)
  (let* ((model-location (railgun-file-name-for-model (railgun-prompt-for-model)))
         (dir (replace-regexp-in-string "/models/" "/controllers" model-location))
         (controller (replace-regexp-in-string ".rb$" "_controller.rb" dir)))
    (find-file controller)))


(defun railgun-prompt-for-model ()
  (let* ((model (railgun-class-from-file-name (buffer-file-name)))
         (initial-value (if (is-railgun-model-p) model))
         (input (ido-completing-read "Model: " (railgun-models) nil t initial-value)))
    (if (string= "" input) model input)))


;; resources

(defun railgun-model-files ()
  (all-files-under-dir-recursively (concat (eproject-root) "app/models") ".rb$"))

(defvar railgun/models-alist nil)
(defun railgun-models-alist ()
  (or railgun/models-alist
      (setq railgun/models-alist (mapcar 'railgun-class-and-table-name
                                       (railgun-model-files)))))

(defvar railgun/model-files-alist nil)
(defun railgun-model-files-alist ()
  (or railgun/model-files-alist
      (setq railgun/model-files-alist (mapcar 'railgun-class-and-file-name
                                            (railgun-model-files)))))

(defun railgun-clear-model-caches ()
  (interactive)
  (setq railgun/models-alist nil)
  (setq railgun/model-files-alist nil))

(defun railgun-models ()
  (mapcar 'car (railgun-models-alist)))

;; parsing

(defun is-railgun-model-p ()
  (let ((model-regexp (concat "^" (eproject-root) "app/models")))
    (string-match model-regexp (buffer-file-name))))

(defun railgun-class-and-file-name (file-name)
  (let ((class (railgun-class-from-file-name file-name)))
    `(,class . ,file-name)))

(defun railgun-class-and-table-name (file-name)
  (let ((class (railgun-class-from-file-name file-name))
        (table (railgun-table-name-from-file-name file-name)))
    `(,class . ,table)))

(defun railgun-table-name-from-file-name (file-name &optional ns)
  "get an underscored version of the current models name, passing in what to use as namespace delimiter"
  (let* ((delim-with (or ns "_"))
         (model-dir (concat (eproject-root) "app/models/"))
         (model (replace-regexp-in-string model-dir "" file-name))
         (filename (replace-regexp-in-string "/" delim-with model)))
    (replace-regexp-in-string ".rb$" "" filename)))

(defun railgun-class-from-file-name (file-name)
  (let* ((table-name (railgun-table-name-from-file-name file-name "::"))
         (capitalized (capitalize table-name)))
    (replace-regexp-in-string "_" "" capitalized)))

(defun railgun-table-name-for-model (model)
  (pluralize-string (cdr (assoc model (railgun-models-alist)))))

(defun railgun-file-name-for-model (model)
  (cdr (assoc model (railgun-model-files-alist))))

(provide 'railgun)
