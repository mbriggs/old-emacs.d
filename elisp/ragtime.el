(defvar *ragtime-time-format* "%Y%m%d%H%M%S")

(defun ragtime/timestamp (time)
  (format-time-string *ragtime-time-format* time))

(defun ragtime/title (name)
  (replace-regexp-in-string " " "-" (downcase name)))

(defun ragtime/migration-name (name dir time)
  (concat (projectile-project-root) "src/migrations/" (ragtime/timestamp time) "-" (ragtime/title name) "." dir ".sql"))

(defun ragtime-create-migration ()
  (interactive)

  (let ((name (read-from-minibuffer "Migration Name: "))
        (time (current-time)))
    (find-file-other-window (ragtime/migration-name name "down" time))
    (find-file-other-window (ragtime/migration-name name "up" time))))

(provide 'ragtime)
