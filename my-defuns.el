(defun new-line-in-normal-mode ()
  "make a new line without moving the cursor or leaving normal mode"
  (interactive)
  (evil-set-marker ?z)
  (evil-insert-newline-below)
  (evil-force-normal-state)
  (evil-goto-mark ?z))

(defun extract-variable ()
  (interactive)
  (let ((name (read-from-minibuffer "Variable name: ")))
    (evil-change (region-beginning) (region-end))
    (insert name)
    (evil-open-above 1)
    (insert (concat name " ="))
    (evil-paste-after 1))
    (indent-for-tab-command)
  (evil-normal-state))

(defun inline-variable ()
  (interactive)
  (let ((name (current-word)))
    (re-search-forward "= ")
    (let ((value (buffer-substring (point) (point-at-eol))))
      (kill-whole-line)
      (search-forward name)
      (replace-match value)))
  (evil-normal-state))

;; foo = 1.4 * whatever()
;; blah.blah(foo)

(defun schema ()
  (interactive)

  (let* ((name (replace-regexp-in-string ".rb$" "s" (buffer-name)))
         (root (eproject-root))
         (regexp (concat "create_table \"" name "\"")))

    (find-file (concat root "db/schema.rb"))
    (or (re-search-forward regexp nil t)
        (re-search-backward regexp nil t))
    (message (concat "looking for " name))))

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

(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

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

(defun build-gtags ()
  (interactive)
  (message "building gtags")
  (let ((root (eproject-root)))
    ; really REALLY need to fix the rtags ruby 1.9.3 issue
    (shell-command (concat "(rvm ree-1.8.7-2011.03 && cd " root " && gtags)"))
    (message "tags loaded")))

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
      (progn (set-shoulda-command-to-proj-root)
             (shoulda-verify))
    (rspec-verify)))

(defun test-verify-all ()
  (interactive)
  (if (eproject-attribute :use-shoulda)
      (progn (set-shoulda-command-to-proj-root)
             (shoulda-verify-all))
    (rspec-verify-all)))

(defun test-verify-single ()
  (interactive)
  (if (eproject-attribute :use-shoulda)
      (progn (set-shoulda-command-to-proj-root)
             (shoulda-verify-single))
    (rspec-verify-single)))

(defun test-toggle ()
  (interactive)
  (if (eproject-attribute :use-shoulda)
      (rtt/toggle-test-and-implementation)
      (rspec-toggle-spec-and-target)))

(defun solarized-find-color (name)
  (let ((index (if window-system
                   (if solarized-degrade
                       3
                     (if solarized-srgb
                         1
                       2))
                 (if (= solarized-termcolors 256)
                     3
                   4))))
    (nth index (assoc name solarized-colors))))

(setq sol-base03    (solarized-find-color 'base03)
      sol-base02    (solarized-find-color 'base02)
      sol-base01    (solarized-find-color 'base01)
      sol-base00    (solarized-find-color 'base00)
      sol-base0     (solarized-find-color 'base0)
      sol-base1     (solarized-find-color 'base1)
      sol-base2     (solarized-find-color 'base2)
      sol-base3     (solarized-find-color 'base3)
      sol-yellow    (solarized-find-color 'yellow)
      sol-orange    (solarized-find-color 'orange)
      sol-red       (solarized-find-color 'red)
      sol-magenta   (solarized-find-color 'magenta)
      sol-violet    (solarized-find-color 'violet)
      sol-blue      (solarized-find-color 'blue)
      sol-cyan      (solarized-find-color 'cyan)
      sol-green     (solarized-find-color 'green))

(provide 'my-defuns)
