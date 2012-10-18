;(defun iedit-dwim (arg)
;  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
;  (interactive "P")
;  (if arg (iedit-mode)
;    (save-excursion
;      (save-restriction
;        (widen)
;
;        (if (eq major-mode 'ruby-mode) (narrow-to-ruby-block)
;          (narrow-to-defun))
;
;        (if iedit-mode (iedit-done)
;          (iedit-start (current-word)))))))

(defun narrow-to-ruby-block ()
  (save-excursion
    (let ((start (progn (ruby-beginning-of-block) (point)))
          (end (progn (ruby-end-of-block) (point))))
      (narrow-to-region start end))))

(defun presentation-mode ()
  (interactive)
  (color-theme-scintilla)
  (set-frame-font "Menlo-22" t))

(defun ruby-onenine-ify-line-hashes ()
  (interactive)
  (ruby-onenine-ify-hashes (point-at-bol) (point-at-eol)))

(defun ruby-onenine-ify-region-hashes ()
  (interactive)
  (ruby-onenine-ify-hashes (region-beginning) (region-end)))

(defun ruby-onenine-ify-buffer-hashes ()
  (interactive)
  (ruby-onenine-ify-hashes (point-min) (point-max)))

(defun ruby-onenine-ify-hashes (point-start point-end)
  (replace-regexp ":\\([a-zA-Z0-9_]+\\) +=>" "\\1:"
                  nil point-start point-end))

(defun newline-anywhere ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun newline-on-previous-line-anywhere ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(defun current-line-number ()
  (+ 1 (count-lines 1 (point))))

(defun minimap-visible-p ()
  (and minimap-bufname
       (get-buffer minimap-bufname)
       (get-buffer-window (get-buffer minimap-bufname))))

(defun toggle-minimap ()
  (interactive)
  (if (minimap-visible-p)
      (minimap-kill)
    (minimap-create)))

(defun ruby-debug-puts ()
  (interactive)
  (beginning-of-line)
  (insert (concat "p \" !!!!!!!!!!!!!!!!!!!!!!!!!!!! [ LINE " (number-to-string (current-line-number)) " ]\""))
  (indent-for-tab-command)
  (newline)
  (indent-for-tab-command)
  (insert "p \"DBG: \"")
  (newline)
  (indent-for-tab-command)
  (insert "p \" ==================>> END <<=====================\"")
  (newline)
  (indent-for-tab-command)
  (previous-line)
  (previous-line)
  (end-of-line)
  (backward-char))

(defun js-alert-line ()
  (interactive)
  (newline)
  (indent-for-tab-command)
  (insert
   (concat "alert(\"DBG: Made it to line '" (number-to-string (current-line-number)) "'!!\")")))

(defun js-log-line ()
  (interactive)
  (newline)
  (indent-for-tab-command)
  (insert
   (concat "console.log(\"DBG: Made it to line '" (number-to-string (current-line-number)) "'!!\")")))

(defun js-console-log ()
  (interactive)
  (newline)
  (indent-for-tab-command)
  (insert "console.log()")
  (backward-char 1))

(defun send-current-line-to-next-window ()
  "Send current line to next window"
  (interactive)
  (let ((current-line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (target (window-buffer (next-window))))
    (with-current-buffer target
      (insert current-line))))

(defun reset-current-dir ()
  (interactive)
  (let ((dir (file-name-directory (buffer-file-name))))
    (cd dir)
    (message (concat "Set the current buffer directory to " dir))))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun bundle-install ()
  (interactive)
  (let ((out (get-buffer-create "*bundler*"))
        (cmd (concat "cd " (railway-root) " && bundle install --binstubs")))
    (rvm-autodetect-ruby)
    (popwin:popup-buffer out)
    (toggle-buffer-tail out "on")
    (shell-command cmd out)
    (message "bundle completed")))

(defun my-clear-all-caches ()
  (interactive)
  (textmate-clear-cache)
  (ra/clear-caches)
  (rg-clear-caches)
  (yas/reload-all))

(defun my-delete-backwards ()
  (interactive)
  (delete-region (point) (progn (evil-backward-word-begin) (point))))

(defun insert-js-function ()
  (interactive)
  (insert "function()")
  (backward-char))

(defun add-to-js-globals ()
  (interactive)
  (let ((var (word-at-point)))
    (save-excursion
      (beginning-of-buffer)
      (when (not (string-match "^/\\* global " (current-line)))
          (newline)
          (previous-line)
          (insert "/* global */"))
      (while (not (string-match "*/" (current-line)))
        (next-line))
      (end-of-line)
      (delete-char -2)
      (insert (concat var " */")))))

(defun new-line-in-normal-mode ()
  "make a new line without moving the cursor or leaving normal mode"
  (interactive)
  (save-excursion
    (evil-insert-newline-below)
    (evil-force-normal-state)))

(defun semi-colonize ()
  (interactive)
  (query-replace-regexp " *.+[^;,{}\n]$" "\\&;"))

(defun format-json ()
  (interactive)
  (let ((cmd "python -mjson.tool"))
    (shell-command-on-region (region-beginning) (region-end) cmd nil t)))

(defun copy-to-end-of-line ()
  (interactive)
  (copy-region-as-kill (point) (point-at-eol)))

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
    (shell-command (concat "ctags -e -R --extra=+q --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

(defun build-gtags ()
  (interactive)
  (message "building gtags")
  (let ((root (eproject-root)))
    (shell-command (concat "(cd " root " && gtags)"))
    (message "tags loaded")))

(defun fix-buffer-directory ()
  (interactive)
  (if buffer-file-name
      (setq default-directory
            (file-name-directory buffer-file-name))))

(defun my-find-tag ()
  (interactive)
  (if (file-exists-p (concat (eproject-root) "TAGS"))
      (visit-project-tags)
    (build-ctags))
  (etags-select-find-tag-at-point))

(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))


(defvar *use-spork* nil)
(defun toggle-use-spork ()
  (interactive)
  (setq *use-spork* (if *use-spork* nil t))
  (message (concat (unless *use-spork* "not ") "using spork")))

(defadvice shoulda-run-single-file (around set-shoulda-command)
  (let* ((runner (if *use-spork* "testdrb" "ruby"))
         (shoulda-command (concat "(cd " (eproject-root) " && "
                                         runner " \"%f\" %o)")))
    ad-do-it))
(ad-activate 'shoulda-run-single-file)

(require 'rspec-mode)

(defun rspec-runner ()
  (concat "cd " (eproject-root) " && bundle exec rspec"))

(defun test-verify ()
  (interactive)
  (if (rg-spec?) (rspec-verify) (shoulda-verify)))

(defun test-verify-all ()
  (interactive)
  (rspec-verify-all))

(defun test-verify-single ()
  (interactive)
  (if (rg-spec?) (rspec-verify-single) (shoulda-verify-single)))

(defvar solarized-colors
  ;; name    sRGB      Gen RGB   
  '((base03  "#002b36" "#042028")
    (base02  "#073642" "#0a2832")
    (base01  "#586e75" "#465a61")
    (base00  "#657b83" "#52676f")
    (base0   "#839496" "#708183")
    (base1   "#93a1a1" "#81908f")
    (base2   "#eee8d5" "#e9e2cb")
    (base3   "#fdf6e3" "#fcf4dc")
    (yellow  "#b58900" "#a57705")
    (orange  "#cb4b16" "#bd3612")
    (red     "#dc322f" "#c60007")
    (magenta "#d33682" "#c61b6e")
    (violet  "#6c71c4" "#5859b7")
    (blue    "#268bd2" "#2075c7")
    (cyan    "#2aa198" "#259185")
    (green   "#859900" "#728a05"))
  "This is a table of all the colors used by the Solarized color theme. Each
   column is a different set, one of which will be chosen based on term
   capabilities, etc.")

(defun solarized-find-color (name)
  (let ((index (if (eq system-type 'darwin) 2 1)))
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

(defface  my-parens       `((((class color)) (:foreground "#BEA75D"))) "custom parens"  :group 'faces)
(defface  my-braces       `((((class color)) (:foreground ,sol-blue  ))) "custom braces"  :group 'faces)
(defface  my-brackets     `((((class color)) (:foreground ,sol-violet))) "custom brackets" :group 'faces)
(defface  my-dot          `((((class color)) (:foreground "#BEA75D"))) "custom brackets" :group 'faces)
(defface  my-semis        `((((class color)) (:foreground "#BEA75D"))) "custom semicolons" :group 'faces)
(defface  my-double-quote `((((class color)) (:foreground ,sol-red))) "custom special" :group 'faces)

(defvar tweak-syntax-blacklist '(magit-status-mode
                                 magit-log-mode
                                 magit-commit-mode
                                 magit-branch-manager-mode
                                 deft-mode
                                 ruby-mode
                                 gfm-mode
                                 org-mode
                                 erc-mode))

(defun tweak-syntax ()
  (if (not (member major-mode tweak-syntax-blacklist))
      (mapcar (lambda (x) (font-lock-add-keywords nil x))
              '((("#?['`]*(\\|)" . 'my-parens))
                (("#?\\^?{\\|}" . 'my-braces))
                (("\\[\\|\\]" . 'my-brackets))
                (("\\." . 'my-dot))
                (("; *$" . 'my-semis))
                (("#?\"" 0 'my-double-quote prepend))
                (("#?\'" 0 'my-double-quote prepend))
                (("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 'font-lock-warning-face t))))))



(add-hook 'after-change-major-mode-hook 'tweak-syntax)

(provide 'my-defuns)
