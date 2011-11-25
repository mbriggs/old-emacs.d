(defun new-line-in-normal-mode ()
  "make a new line without moving the cursor or leaving normal mode"
  (interactive)
  (evil-set-marker ?z)
  (evil-insert-newline-below)
  (evil-force-normal-state)
  (evil-goto-mark ?z))

(defun format-json ()
  (interactive)
  (let ((cmd "python -mjson.tool"))
    (shell-command-on-region (region-beginning) (region-end) cmd nil t)))

(defun copy-to-end-of-line ()
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun command-t ()
  (interactive)
  (fuzzy-find-project-root (eproject-root))
  (fuzzy-find-in-project))

(defun hex-codes (limit)
  (remove-overlays (point) limit 'fontify-hex-colors t)
  (while (re-search-forward "\\(#[[:xdigit:]]\\{6\\}\\)" limit t)
    (let ((ov (make-overlay (match-beginning 0)
                            (match-end 0))))
      (overlay-put ov 'face  (list :background (match-string 1)))
      (overlay-put ov 'fontify-hex-colors t)
      (overlay-put ov 'evaporate t)))
  ;; return nil telling font-lock not to fontify anything from this
  ;; function
  nil)


(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file name new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))


(defun build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (eproject-root)))
    (shell-command (concat "ctags -e -R --exclude=db --exclude=test -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

(defun my-find-tag ()
  (interactive)
  (visit-project-tags)
  (etags-select-find-tag-at-point))

(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

(defun test-verify ()
  (interactive)
  (if (eproject-attribute :use-shoulda)
      (shoulda-verify)
      (rspec-verify)))

(defun test-verify-all ()
  (interactive)
  (if (eproject-attribute :use-shoulda)
      (shoulda-verify-all)
      (rspec-verify-all)))

(defun test-verify-single ()
  (interactive)
  (if (eproject-attribute :use-shoulda)
      (shoulda-verify-single)
      (rspec-verify-single)))

(defun test-toggle ()
  (interactive)
  (if (eproject-attribute :use-shoulda)
      (rtt/toggle-test-and-implementation)
      (rspec-toggle-spec-and-target)))

(provide 'my-defuns)