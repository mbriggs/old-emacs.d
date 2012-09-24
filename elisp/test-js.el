;;; test-js.el

;; Copyright (C) 2012 Matt Briggs

;; Author: Matt Briggs <matt@mattbriggs.net>
;; Keywords: navigation
;; Version: 1

;; easily switch between tests and implementation, and create tests/specs

;; depends on joseph-file-util.el being already loaded

(defun tjs-toggle-test-and-implementation ()
  (interactive)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (matches (if (tjs-on-implementation-p)
                      (tjs-find-test file)
                    (tjs-find-implementation file)))
         (matched (length matches)))

    (cond ((= 0 matched)
           (message "could not find a matching test"))
          ((= 1 matched)
           (find-file (first matches)))
          (t
           (find-file
            (let ((paths (mapcar 'tjs-build-path matches)))
              (find-file (tjs-choose-path paths))))))))

(defun tjs-choose-path (paths)
  (let ((choice (ido-completing-read
                 "Found Multiple Possibilities: "
                 (mapcar 'car paths))))
    (cdr (assoc choice paths))))

(defun tjs-create-test ()
  (interactive)
  (let ((target (read-from-minibuffer "Create Test: " (tjs-current-file))))
    (find-file (concat (tjs-path) "tests/" target ".js"))))

(defun tjs-create-spec ()
  (interactive)
  (let ((target (read-from-minibuffer "Create Spec: " (tjs-current-file))))
    (find-file (concat (tjs-path) "spec/" target ".js"))))

(defun tjs-current-file ()
  (let ((no-extension (replace-regexp-in-string ".js$" "" (buffer-file-name))))
    (replace-regexp-in-string (tjs-path) "" no-extension)))

(defun tjs-build-path (file)
  `(,(tjs-relative-path file) . ,file))

(defun tjs-path (&optional path)
  (if path (concat (tjs-root) path)
    (tjs-root)))

(defun tjs-relative-path (file)
  (let ((no-extension (replace-regexp-in-string ".js$" "" file)))
    (replace-regexp-in-string (tjs-root) "" no-extension)))

(defun tjs-find-implementation (file)
  (tjs-find-matching-files file (tjs-files)))

(defun tjs-find-test (file)
  (let ((underscore-spec (tjs-set-file-postfix file "_spec"))
        (dot-spec (tjs-set-file-postfix file ".spec"))
        (camel-spec (tjs-set-file-postfix file "Spec"))
        (specs (tjs-specs))
        (tests (tjs-tests)))

    (append (tjs-find-matching-files file tests)
            (tjs-find-matching-files underscore-spec specs)
            (tjs-find-matching-files dot-spec specs)
            (tjs-find-matching-files camel-spec specs))))

(defun tjs-set-file-postfix (file-name postfix)
  (replace-regexp-in-string "^\\(.+\\)\\.\\([a-z]+\\)$"
                            (concat "\\1" postfix ".\\2")
                            file))

(defun tjs-find-matching-files (file-name file-list)
  (delq nil
        (mapcar (lambda (file)
                  (let ((target (file-name-nondirectory file)))
                    (and (string= file-name target) file)))
                file-list)))

(defun tjs-on-implementation-p ()
  (not (string-match-p "/\\(tests\\|spec\\)/" (buffer-file-name))))

(defun tjs-on-test-p ()
  (string-match-p "/test/" (buffer-file-name)))

(defun tjs-on-spec-p ()
  (string-match-p "/spec/" (buffer-file-name)))

(defun tjs-files ()
  (all-files-under-dir-recursively (tjs-root)
                                   ".js$" t
                                   "/\\(tests\\|spec\\|node_modules\\)/" t))
(defun tjs-tests ()
  (all-files-under-dir-recursively (tjs-path "tests/")
                                   ".js$" nil))

(defun tjs-specs ()
  (all-files-under-dir-recursively (tjs-path "spec/")
                                   ".js$" nil))

;; stolen from rinari
(defun tjs-root (&optional dir home)
  (or dir (setq dir default-directory))
  (if (file-exists-p (expand-file-name "package.json" dir))
      dir
    (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      ;; regexp to match windows roots, tramp roots, or regular posix roots
      (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:/?$\\|^/$\\)" dir)
        (tjs-root new-dir)))))

(provide 'test-js)