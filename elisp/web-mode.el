;;; web-mode.el --- major mode for editing HTML templates (PHP/JSP/ASP/JS/CSS)

;; Copyright (C) 2011, 2012 François-Xavier Bois

;; =========================================================================
;; This work is sponsored by KerniX : Digital Agency (Web & Mobile) in Paris
;; =========================================================================

;; Version: 2.99
;; Author: François-Xavier Bois <fxbois AT Google Mail Service>
;; Maintainer: François-Xavier Bois
;; Created: July 2011
;; Keywords: Web Template HTML PHP JavaScript CSS JS JSP ASP 
;; URL: http://github.com/fxbois/web-mode
;;      http://web-mode.org

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(eval-when-compile 
  (require 'cl))

(defgroup web-mode nil
  "Major mode for editing mixed HTML templates (PHP/JSP/ASPX/JS/CSS)."
  :version "2.99"
  :group 'languages)

(defgroup web-mode-faces nil
  "Faces for syntax highlighting."
  :group 'web-mode
  :group 'faces)

(defconst web-mode-debug nil
  "t if in debug mode.")

(defcustom web-mode-html-offset 2
  "HTML indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-css-offset 2
  "CSS indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-script-offset 2
  "General script indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-php-offset web-mode-script-offset
  "PHP indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-javascript-offset web-mode-script-offset
  "JavaScript indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-java-offset web-mode-script-offset
  "Java indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-asp-offset web-mode-script-offset
  "ASP indentation offset."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-indent-style 1
  "Indentation style (1=low indent, 2=lax indent)."
  :type 'integer
  :group 'web-mode)

(defcustom web-mode-autocompletes-flag (display-graphic-p)
  "Handle autocompletes."
  :type 'bool
  :group 'web-mode)

(defface web-mode-preprocessor-face
  '((t :inherit font-lock-preprocessor-face))
  "Face for preprocessor."
  :group 'web-mode-faces)

(defface web-mode-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for builtins."
  :group 'web-mode-faces)

(defface web-mode-doctype-face
  '((t :foreground "Grey"))
  "Face for HTML doctype."
  :group 'web-mode-faces)

(defface web-mode-html-tag-face
  '((t :foreground "Snow4"))
  "Face for HTML tags."
  :group 'web-mode-faces)

(defface web-mode-html-attr-name-face
  '((t :foreground "Snow3"))
  "Face for HTML attribute names."
  :group 'web-mode-faces)

(defface web-mode-html-attr-value-face
  '((t :inherit font-lock-string-face))
  "Face for HTML attribute values."
  :group 'web-mode-faces)

(defface web-mode-css-rule-face
  '((t :foreground "orchid3"))
  "Face for CSS rules."
  :group 'web-mode-faces)

(defface web-mode-css-pseudo-class-face
  '((t :foreground "plum2"))
  "Face for CSS pseudo-classes."
  :group 'web-mode-faces)

(defface web-mode-css-at-rule-face
  '((t :inherit font-lock-builtin-face))
  "Face for CSS at-rules."
  :group 'web-mode-faces)

(defface web-mode-css-prop-face
  '((t :foreground "Pink3"))
  "Face for CSS props."
  :group 'web-mode-faces)

(defface web-mode-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for variable names."
  :group 'web-mode-faces)

(defface web-mode-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for function names."
  :group 'web-mode-faces)

(defface web-mode-string-face
  '((t :inherit font-lock-string-face))
  "Face for strings."
  :group 'web-mode-faces)

(defface web-mode-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments."
  :group 'web-mode-faces)

(defface web-mode-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for language constants."
  :group 'web-mode-faces)

(defface web-mode-type-face
  '((t :inherit font-lock-type-face))
  "Face for language types."
  :group 'web-mode-faces)

(defface web-mode-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for language keywords."
  :group 'web-mode-faces)

(defface web-mode-folded-face
  '((t :underline t))
  "Overlay face for folded."
  :group 'web-mode-faces)

(defvar web-mode-tag-regexp "<\\(/?[[:alpha:]@#][[:alnum:]:_]*\\)"
  "Regular expression for HTML/XML tag.")

(defvar web-mode-file-type "html"
  "Buffer file type.")

(defvar web-mode-comments-invisible nil
  "Comments visbility.")

(defvar web-mode-block-beg nil
  "Beg of current block.")

(defvar web-mode-hook nil
  "List of functions to be executed with web-mode.")

(defvar web-mode-server-language "php"
  "Server script language.")

(defvar web-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table in use in web-mode buffers.")

(defvar web-mode-map
  (let ((keymap (make-sparse-keymap)))

    (define-key keymap (kbd "C-c C-;") 'web-mode-comment-uncomment)
    (define-key keymap (kbd "C-c C-(") 'web-mode-fetch-opening-paren)
    (define-key keymap (kbd "C-c C-a") 'web-mode-indent-buffer)
    (define-key keymap (kbd "C-c C-b") 'web-mode-beginning-of-element)
    (define-key keymap (kbd "C-c C-d") 'web-mode-delete-element)
    (define-key keymap (kbd "C-c C-f") 'web-mode-toggle-folding)
    (define-key keymap (kbd "C-c C-i") 'web-mode-insert)
    (define-key keymap (kbd "C-c C-j") 'web-mode-duplicate-element)
    (define-key keymap (kbd "C-c C-n") 'web-mode-match-tag)
    (define-key keymap (kbd "C-c C-p") 'web-mode-parent-html-element)
    (define-key keymap (kbd "C-c C-r") 'web-mode-rename-element)
    (define-key keymap (kbd "C-c C-s") 'web-mode-select-element)
    
    (define-key keymap (kbd "C-^") 
      (lambda ()
        (interactive)
        (message "reload web-mode")
        (web-mode-reload)
        ))
    
    (define-key keymap (kbd "C-$") 
      (lambda ()
        (interactive)
        (message 
         "block-language=%S block-side=%S block-type=%S face=%S web-mode-is-csss=%S" 
         (get-text-property (point) 'block-language)
         (get-text-property (point) 'block-side)
         (get-text-property (point) 'block-type)
         (get-text-property (point) 'face)
         (web-mode-is-csss (point)))
        ))
    
    keymap)
  "Keymap for `web-mode'.")

;;;###autoload
(define-derived-mode web-mode prog-mode "Web"
  "Major mode for editing mixed HTML Templates."

;;  (make-local-variable 'font-lock-extra-managed-props)
;;  (make-local-variable 'font-lock-extend-region-functions)
  (make-local-variable 'font-lock-keywords)  
  (make-local-variable 'font-lock-multiline)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-tabs-mode)  
  (make-local-variable 'require-final-newline)
  (make-local-variable 'web-mode-autocompletes-flag)
  (make-local-variable 'web-mode-block-beg)
  (make-local-variable 'web-mode-file-type)
  (make-local-variable 'web-mode-server-language)

;;  (make-local-variable 'font-lock-extend-after-change-region-function)
;;  (setq font-lock-extend-after-change-region-function 'web-mode-extend-after-change-region)

  (cond

   ((string-match-p "\\.xml$" (buffer-file-name))
    (setq web-mode-file-type "xml"
          font-lock-defaults '(web-mode-html-font-lock-keywords t t nil nil))
    (add-hook 'font-lock-extend-region-functions 'web-mode-font-lock-extend-region nil t))

   ((string-match-p "\\.css$" (buffer-file-name))
    (setq web-mode-file-type "css"
          web-mode-autocompletes-flag nil
          font-lock-defaults '(web-mode-css-font-lock-keywords t t nil nil))
    (add-hook 'font-lock-extend-region-functions 'web-mode-font-lock-extend-css-region nil t))
   
   (t
    (setq web-mode-file-type "html"
          font-lock-defaults '(web-mode-html-font-lock-keywords t t nil nil))
    (add-hook 'font-lock-extend-region-functions 'web-mode-font-lock-extend-region nil t))
   
   )
  
  (cond
   
   ((string-match-p "\\.as[cp]x$" (buffer-file-name))
    (setq web-mode-server-language "asp"))

   ((string-match-p "\\.erb$" (buffer-file-name))
    (setq web-mode-server-language "ruby")
    (font-lock-add-keywords 'web-mode-html-font-lock-keywords
                            '("<%\\(#\\|--\\)\\(.\\|\n\\)*?%>" 0 'web-mode-comment-face t t)))
   )

;;  (setq font-lock-extra-managed-props '(server-script block-kind)
  (setq font-lock-multiline nil
        indent-line-function 'web-mode-indent-line
        indent-tabs-mode nil
        require-final-newline nil)

  (if (not (string= web-mode-file-type "css"))
      (add-hook 'after-change-functions 'web-mode-on-after-change t t)
    )

  (web-mode-scan (point-min) (point-max))

  )

(defun web-mode-scan-init ()
  "Trigger the first scan."
  (interactive))

(defun web-mode-scan (beg end)
  "Identify blocks (JS/CSS/PHP/ASP/JSP/string/comment)."
  (interactive)
;;  (message "scanning buffer from %d to %d" beg end)
  (with-silent-modifications
   (save-excursion
     (let ((inhibit-modification-hooks t)
           (inhibit-point-motion-hooks t)
           (inhibit-quit t))
       (remove-text-properties beg end '(block-language nil client-language nil server-language nil block-side nil client-side nil server-side nil block-type nil client-type nil server-type nil))
       (web-mode-scan-client-side beg end)
       (web-mode-scan-server-side beg end)
       ))))
  
(defconst web-mode-client-side-regexps
  (list
   '("\"[^\"]*\"\\|'[^']*'" string)
   '("/\\*\\(.\\|\n\\)*?\\*/" comment)
   '("//.*$" comment)
   )
  "Client side regexps")

(defun web-mode-scan-client-side (beg end)
  "Scan client side blocks (JS / CSS / HTML Comments) and identifies strings and comments."
  (save-excursion
    (let (open close chunk counter regexp props closing-string type pair
               (l (length web-mode-client-side-regexps)))
      (goto-char beg)
      (while (re-search-forward "<\\([!#%]--\\|style[^>]*>\\|script[^>]*>\\)" end t)
        (setq open (point)
              close nil
              pos nil
              counter 0
              chunk (substring (match-string 0) 0 3))
        
        (cond
         
         ((string= "<sc" chunk) 
          (setq closing-string "</script>"
                props '(block-language js client-language js block-side client client-side t)))
         
         ((string= "<st" chunk) 
          (setq closing-string "</style>" 
                props '(block-language css client-language css block-side client client-side t)))
         
         ((or (string= "<!-" chunk) (string= "<#-" chunk)) 
          (setq closing-string "-->" 
                open (match-beginning 0)
                props (if (string= "<!-" chunk)
                          '(block-side client client-side t block-type comment client-type comment)
                        '(block-side server server-side t block-type comment server-type comment))))
         
         ((string= "<%-" chunk) 
          (setq closing-string "%>" 
                open (match-beginning 0)
                props '(block-side client block-type comment server-type comment)))
         
         );;cond
        
        (when (and closing-string
                   (search-forward closing-string end t))
;;          (setq close (if (string= "<!-" chunk) (match-end 0) (match-beginning 0))
          (setq close (if (member chunk '("<!-" "<#-" "<%-")) (match-end 0) (match-beginning 0))
                pos (point))
;;          (message "block=%s" (buffer-substring open close))
          (add-text-properties open close props)        
          );; when
        
        (when (and close (or (string= "<sc" chunk) (string= "<st" chunk)))
          (while (< counter l)
            (goto-char open)
            (setq pair (nth counter web-mode-client-side-regexps))
            (setq regexp (nth 0 pair)
                  type (nth 1 pair))
            (setq props (if (eq type 'string) 
                            '(block-type string client-type string) 
                          '(block-type comment client-type comment)))
            (setq counter (1+ counter))
            (while (re-search-forward regexp close t)
              (when (null (get-text-property (match-beginning 0) 'client-type))
                (add-text-properties (match-beginning 0) (match-end 0) props)
;;                (message "%S -> %s" (get-text-property (match-beginning 0) 'block-type) (buffer-substring (match-beginning 0) (match-end 0)))
                );;when
              )
            )
          )
        
        (if pos (goto-char pos))
        
        );;while
      
      )))

(defconst web-mode-server-side-regexps
  (list
   '("\"[^\"]*\"\\|'[^']*'" string)
   '("/\\*\\(.\\|\n\\)*?\\*/" comment)
   '("//.*$" comment)
   )
  "Server side regexps")

(defun web-mode-scan-server-side (beg end)
  "Scan server side blocks (PHP / JSP / ASP) and identifies strings and comments."
  (save-excursion
    (let (open close chunk counter ms regexp props closing-string type pair
               (do-loop t)
               (l (length web-mode-client-side-regexps)))
      (goto-char beg)
      (while (re-search-forward "\\(<\\?php\\|<\\?=\\|<%[@]?\\|[$#]{\\|{[#{%]\\)" end t)
        (setq open (point)
              close nil
              pos nil
              counter 0
              sub2 (substring (match-string 0) 0 2)
              ms (match-string 0))
        
        (cond
         
         ((string= "<?" sub2) 
          (setq closing-string "?>"
                props '(block-language php server-language php block-side server server-side t)))
         
         ((string= "<%@" ms) 
          (setq closing-string "%>" 
                props '(block-language directive server-language directive block-side server server-side t)))
         
         ((string= "<%" sub2) 
          (setq closing-string "%>"
                props (if (string= web-mode-server-language "asp")
                          '(block-language asp server-language asp block-side server server-side t)
                        '(block-language jsp server-language jsp block-side server server-side t))))
         
         ((member sub2 '("${" "#{"))
          (setq closing-string "}"
                props '(block-language jsp server-language jsp block-side server server-side t)
                do-loop nil)
          )
         
         ((member sub2 '("{{" "{%"))
          (setq closing-string (if (string= "{{" sub2) "}}" "%}")
                props '(block-side server server-side t)
                do-loop (string= "{%" sub2))
          )
         
         ((string= "{#" sub2)
          (setq closing-string "#}" 
                props '(block-side server server-side t block-type comment server-type comment)
                do-loop nil)
          )
         
         );;cond
        
        (setq open (match-beginning 0))
        
        (if (search-forward closing-string end t)
            (setq close (match-end 0)
                  pos (point))
          (if (string= "<?" sub2)
              (setq close (point-max)
                    pos (point-max)))
          )
        
        (when close
          (add-text-properties open close props)
          (while (and do-loop (< counter l))
            (goto-char open)
            (setq pair (nth counter web-mode-server-side-regexps))
            (setq regexp (nth 0 pair)
                  type (nth 1 pair))
            (setq counter (+ counter 1))
            (while (re-search-forward regexp close t)
              (when (null (get-text-property (match-beginning 0) 'server-type))
                (setq props (if (eq type 'string) 
                                '(block-type string server-type string) 
                              '(block-type comment server-type comment)))
                (add-text-properties (match-beginning 0) (match-end 0) props)
                ;;                (message "%S" (get-text-property (match-beginning 0) 'block-type))
                );;when
              )
            )
          )
        
        (if pos (goto-char pos))
        
        );;while
      
      )))

(defun web-mode-replace-apos ()
  "Replace ' with ’."
  (interactive)
  ;;(web-mode-scan-init)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([[:alpha:]]\\)'\\([[:alpha:]]\\)" nil t)
      (when (and (null (get-text-property (point) 'face))
                 (not (eq (get-text-property (point) 'block-side) 'server))
                 )
        (replace-match "\\1’\\2")))))

(defun web-mode-indent-buffer ()
  "Indent all buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun web-mode-beginning-of-element ()
  "Fetch beginning of element."
  (interactive)
  (let ((continue t)
        (pos nil))
    (save-excursion
      (if (char-equal (char-before) ?<) (backward-char))
      (if (and (looking-at-p "<[[:alpha:]]")
               (not (web-mode-is-csss)))
          (setq pos (point))
        (while continue
          (when (re-search-backward "<[[:alpha:]]" nil t)
            (setq continue (web-mode-is-csss))
            (unless continue (setq pos (point))))
          )))
    (if pos (goto-char pos))
    ))

(defun web-mode-previous-usable-line ()
  "Move cursor to previous non blank/comment/string line and return this line (trimmed).
point is at the beginning of the line."
  (interactive)
  (let ((continue t) 
        (line "")
        (pos (point)))
    (beginning-of-line)
    (while (and continue
                (not (bobp))
                (forward-line -1))
      (if (not (web-mode-is-comment-or-string-line))
          (setq line (web-mode-trim (buffer-substring (point) (line-end-position)))))
      (when (not (string= line "")) (setq continue nil))
      )
    (if (string= line "") 
        (progn
          (goto-char pos)
          nil) 
      line)
    ))

(defun web-mode-is-csss (&optional pos)
  "Detect if point is in a comment, a string or in server script."
  (unless pos (setq pos (point)))
  ;;(web-mode-scan-init)
  (or (get-text-property pos 'server-side)
      (not (null (member (get-text-property pos 'client-type)
                         '(string comment))))))

(defun web-mode-is-comment-or-string (&optional pos)
  "Detect if point is in a comment or in a string."
  (interactive)
  ;;(web-mode-scan-init)
  (not (null (member (get-text-property (or pos (point)) 'block-type) 
                     '(string comment)))))

;;  (memq (get-text-property (or pos (point)) 'face)
;;        '('web-mode-string-face 'web-mode-comment-face))

(defun web-mode-is-comment (&optional pos)
  "Detect if point is in a comment."
  (interactive)
  (unless pos (setq pos (point)))
  (or (eq (get-text-property pos 'server-type) 'comment)
      (eq (get-text-property pos 'client-type) 'comment)))

(defun web-mode-is-comment-or-string-line ()
  "Detect if current line is in a comment or in a string."
  (save-excursion
    (let ((continue t)
          (counter 0))  
      (beginning-of-line)
      (while (and continue (not (eolp)))
        (if (web-mode-is-comment-or-string)
            (setq counter (+ counter 1))
          (when (not (char-equal (following-char) ?\s))
            (setq continue nil
                  counter 0))
          );;if
        (forward-char)
        );;while
      (> counter 0)
      )))

(defun web-mode-in-block (open close)
  "Detect if point is in a block delimited by open and close."
  (save-excursion
    (let (line-current line-open line-close)      
      (setq line-current (web-mode-current-line-number))
      (and (search-backward open nil t)
           (setq web-mode-block-beg (+ (point) (length open)))
           (setq line-open (web-mode-current-line-number))
           (search-forward close nil t)
           (setq line-close (web-mode-current-line-number))
           (not (eq line-open line-close))
           (>= line-close line-current)
           ))))

(defun web-mode-in-server-block (language)
  "Detect if point is in a server (PHP/JSP/ASP/directive) block."
  ;;(web-mode-scan-init)  
  (save-excursion
    (let ((pos (point)))
      (and (eq (get-text-property pos 'server-language) language)
           (progn
             (setq web-mode-block-beg (previous-single-property-change pos 'server-language))
             t)
           )
      )))

(defun web-mode-in-client-block (language)
  "Detect if point is in a client (CSS/JS) block."
  ;;(web-mode-scan-init)
  (save-excursion
    (let ((pos (point)))
      (and (eq (get-text-property pos 'client-language) language)
           (progn
             (setq web-mode-block-beg (previous-single-property-change pos 'client-language))
             t)
           )
      )))

;;  (eq (get-text-property (point) 'block-language) language))

(defun web-mode-current-line-number (&optional pos)
  "Return line number at point."
  (unless pos (setq pos (point)))
  (let (ret)
    (setq ret (+ (count-lines 1 pos)
                 (if (= (current-column) 0) 1 0)))
    ;;    (message "%d [%d] %s" pos out (web-mode-text-at-point))
    ret))

(defun web-mode-clean-client-line (input)
  "Remove comments and server scripts."
  ;;(web-mode-scan-init)
  (let ((i 0) 
        (out "") 
        (n (length input)))
    (while (< i n)
      (unless (or (get-text-property i 'server-side input)
                  (eq (get-text-property i 'client-type input) 'comment))
        (setq out (concat out (substring-no-properties input i (+ i 1)))))
      (setq i (1+ i)))
;;    (message "[%s] > [%s]" input out)
    (web-mode-trim out)
    ))

(defun web-mode-clean-server-line (input)
  "Remove comments."
  ;;(web-mode-scan-init)
  (let ((i 0) 
        (out "") 
        (n (length input)))
    (while (< i n)
      (unless (eq (get-text-property i 'server-type input) 'comment)
        (setq out (concat out (substring-no-properties input i (+ i 1)))))
      (setq i (1+ i)))
    (web-mode-trim out)
    ))

(defun web-mode-indent-line ()
  "Indent current line according to language."
  (interactive)
  (let ((inhibit-modification-hooks t)
        ;;case-fold-search
        continue
        counter
        cur-line-beg-pos
        cur-column
        cur-first-char
        cur-indentation
        cur-line-number
        cur-line
        cur-point
        in-directive-block
        in-php-block
        in-asp-block
        in-jsp-block
        in-script-block
        in-style-block
        in-html-block
        line-number
        local-offset
        offset
        prev-last-char 
        prev-line 
        pt)

    (save-excursion
      (setq cur-line-beg-pos (line-beginning-position)
            cur-column (current-column)
            cur-line-number (web-mode-current-line-number)
            cur-indentation (current-indentation)
            cur-point (point)
            web-mode-block-beg nil)
;;      (end-of-line)

      (back-to-indentation)
;;      (beginning-of-line)

      (cond
       
       ((string= web-mode-file-type "css")
        (setq in-style-block t)
        (setq local-offset web-mode-css-offset))
       
       ((web-mode-in-server-block 'php)
        (setq in-php-block t)
        (setq local-offset web-mode-php-offset))
       
       ((web-mode-in-client-block 'js)
        (setq in-script-block t)
        (setq local-offset web-mode-javascript-offset))
       
       ((web-mode-in-client-block 'css)
        (setq in-style-block t)
        (setq local-offset web-mode-css-offset))
       
       ((web-mode-in-server-block 'jsp)
        (setq in-jsp-block t)
        (setq local-offset web-mode-java-offset))

       ((web-mode-in-server-block 'directive)
        (setq in-directive-block t))

       ((web-mode-in-server-block 'asp)
        (setq in-asp-block t)
        (setq local-offset web-mode-asp-offset))
       
       (t
        (setq in-html-block t)
        (setq local-offset web-mode-html-offset))
       
       ) ;;cond

;;     (message "php(%S) jsp(%S) js(%S) css(%S) directive(%S) asp(%S) html(%S)" in-php-block in-jsp-block in-script-block in-style-block in-directive-block in-asp-block in-html-block)

;;      (message "block limit = %S" web-mode-block-beg)

      (setq cur-line (web-mode-trim (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
      (setq cur-first-char (if (string= cur-line "") cur-line (substring cur-line 0 1)))
      (setq prev-line (web-mode-previous-usable-line))
     
      (unless (null prev-line)
        
        (cond
         
         ((or in-html-block in-script-block in-style-block)
          (setq prev-line (web-mode-clean-client-line prev-line)))
         
         (t
          (setq prev-line (web-mode-clean-server-line prev-line))
          )
         
         )
        
;;        (message "prev=%s" prev-line)        
        (when (>= (length prev-line) 1)
          (setq prev-last-char (substring prev-line -1)))
        
        );; unless

      (end-of-line)
          
;;      (message "block-limit:%d" web-mode-block-beg)

      (cond ;; switch language
       
       ((null prev-line)
        ;;          (message "nothing to check")
        (setq offset 0)
        )
       
       ((or in-php-block in-jsp-block in-asp-block in-script-block) 
;;        (message "prev=%S" prev-last-char)
        (cond

         ((and in-script-block
               (> web-mode-indent-style 1)
               (string= prev-last-char ">"))
          (search-backward "<script" nil t)
          (setq offset (current-column))
          )

         ((or (and in-script-block
                   (string= prev-last-char ">"))
              (and in-php-block
                   (or (string-match-p "<\\?php$" prev-line)
                       (string-match-p "^\\?>" cur-line)
                       (and (= web-mode-indent-style 1)
                            (string-match-p "^<\\?php[ ]+\\(if\\|else\\|for\\|while\\|end\\|\\}\\)" cur-line))))
              (and in-jsp-block 
                   (string-match-p "<%[!]?$" prev-line))
              (and in-asp-block 
                   (string-match-p "<%$" prev-line)))
          (setq offset 0)
          )
         
         ((string= cur-first-char "}")
          (web-mode-fetch-opening-paren "{" (point) web-mode-block-beg)
          (setq offset (current-indentation))
          )
         
         ((string= cur-first-char "?")
          (web-mode-rsb "[=(]" web-mode-block-beg)
          (setq offset (current-column))
          )
         
         ((string= cur-first-char ":")
          (setq offset (current-indentation))
          )
         
         ((string= prev-last-char ",")
          (web-mode-fetch-opening-paren "(" cur-point web-mode-block-beg)
          (setq offset (+ (current-column) 1))
          )
         
         ((or (string= prev-last-char ".") 
              (string= prev-last-char "+")
              (string= prev-last-char "?") 
              (string= prev-last-char ":"))
          (web-mode-rsb "[=(]" web-mode-block-beg)
          (skip-chars-forward "= (")
          (setq offset (current-column))
          )
         
         ((string= prev-last-char "}")
          (setq offset (current-indentation))
          )

         ((string= prev-last-char ";")
          (if (string-match-p ")[ ]*;$" prev-line)
              (progn 
                (re-search-backward ")[ ]*;" web-mode-block-beg)
                (web-mode-fetch-opening-paren "(" (point) web-mode-block-beg))
            (re-search-backward "\\([=(]\\|^[[:blank:]]*var[ ]*\\)" web-mode-block-beg t))
          (setq offset (current-indentation))
          )
        
         ((string= prev-last-char "{")
          (setq offset (+ (current-indentation) local-offset))
          )
         
         ((and in-php-block
               (string-match-p "\\(->[ ]?[[:alnum:]_]+\\|)\\)$" prev-line))
          (search-backward ">" web-mode-block-beg)
          (setq offset (- (current-column) 1))
          )
         
         (t
          ()
          ;;            (message "script block : unknown")
          )
         
         )) ;; end case script block
       
       (in-directive-block

        (cond
         
         ((string-match-p "^<" cur-line)
          (setq offset 0))

         (t
          (goto-char cur-point)
          (re-search-backward "@ " nil t)
          (re-search-forward "@ [[:alpha:]]+ " nil t)
          (setq offset (current-column)))

         )

        ) ;; directive

       (in-style-block
        
        (goto-char cur-point)
        
        (cond
         
         ((or (string-match-p "\\(^}\\|{\\)" cur-line)
              (not (web-mode-in-block "{" "}")))
          (if (= web-mode-indent-style 1)
              (setq offset 0)
            (search-backward "<style" nil t)
            (setq offset (current-column)))
          )
         
         ((and (not (string= "" cur-line))
               (not (string-match-p "^[[:alpha:]-]+[ ]?:" cur-line)))
          (re-search-backward ":"  web-mode-block-beg)
          (skip-chars-forward ":  ")
          (setq offset (current-column))            
          )
         
         (t
          (if (= web-mode-indent-style 1)
              (setq offset local-offset)
            (search-backward "<style" nil t)
            (setq offset (+ (current-column) local-offset)))
          )
         
         )) ;; end case style block
       
       (t ;; case html block
        
        (cond
         
         ((and (not (string= cur-first-char "<"))
               (string-match-p "\\<[[:alpha:]][[:alnum:]-]+=\".*?\"$" prev-line))
          (re-search-backward "<[[:alpha:]]")
          (re-search-forward "<[[:alpha:]_:]+")
          (skip-chars-forward " ")
          (setq offset (current-column))
          )
                  
         ((and (= web-mode-indent-style 1)
               (not (string= web-mode-file-type "xml"))
               (or (string-match-p "^</?\\(head\\|body\\|meta\\|link\\|title\\|style\\|script\\)" cur-line)
;;                   (string-match-p "^<\\?php[ ]+\\(if\\|else\\|for\\|while\\|end\\|\\}\\)" cur-line)
;;                   (string= "<?php" cur-line)
;;                   (string-match-p "^\\(<\\?php\\|<%\\|[?%]>\\)" cur-line)
                   ))
          (setq offset 0)
          )
         
         ((string-match-p "^</" cur-line)
          (goto-char cur-point)
          (beginning-of-line)
          (re-search-forward "</")
          (web-mode-match-tag)
          (setq offset (current-indentation))
          )
         
         ((or (string= "" cur-line)
              (string-match-p "^<[[:alpha:]]" cur-line))
          (setq continue t
                counter 0)
          (while (and continue
                      (re-search-backward "^[[:blank:]]*</?[[:alpha:]]" nil t))
            (when (and (not (web-mode-is-comment-or-string))
                       (not (looking-at-p "[[:blank:]]*</?\\(script\\|style\\)")))
              (setq counter (1+ counter))
              (setq continue nil)
;;              (unless (char-equal (following-char) ?<) (re-search-forward "<"))
              (back-to-indentation)
;;              (message "pt=%d" (point))
              
;;              (setq myl (buffer-substring (point) cur-line-beg-pos))
              
;;              (message "myl=%s" myl)

              (setq offset (+ (current-indentation) 
                              (if (web-mode-is-opened-element (buffer-substring (point) cur-line-beg-pos))
;;                                       (not (looking-at-p "[[:blank:]]*<body")))
                                  (if (and (= web-mode-indent-style 1) 
                                           (looking-at-p "[[:blank:]]*<body"))
                                      0
                                    local-offset)
                                0)
                              ))
              );;when
            );;while
          (if (eq counter 0) (setq offset 0))
          )
         
         (t
          ()
          )
         
         )) ;; end case html block

       ) ;; end switch language block

      ) ;; save-excursion

    
    (when (and offset
               (not (eq cur-indentation offset))) 
      (setq offset (max 0 offset))  
      (indent-line-to offset))
    
    (if (< (current-column) (current-indentation)) (back-to-indentation))
    
    ) ;; let
  )

(defun web-mode-rsf (regexp &optional limit noerror)
  "re-search-forward not in comment or string."
  (unless limit (setq limit nil))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil))
      )
    ret))

(defun web-mode-rsb (regexp &optional limit noerror)
  "re-search-backward not in comment or string."
  (unless limit (setq limit nil))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil)))
    ret))

(defun web-mode-rsb-html (regexp &optional limit noerror)
  "re-search-backward only in html."
  (unless limit (setq limit nil))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-csss)))
          (setq continue nil)))
    ret))

(defun web-mode-fetch-opening-paren (&optional paren pos limit)
  "Fetch opening paren."
  (interactive)
  (unless paren (setq paren "("))
  (unless pos (setq pos (point)))
  (unless limit (setq limit nil))
;;  (message (web-mode-text-at-point))
  (let ((continue t) 
        (n 0)
        regexp)

    (cond

     ((string= paren "(")
      (setq regexp "[)(]"))

     ((string= paren "{")
      (setq regexp "[}{]"))

     ((string= paren "{")
      (setq regexp "[\]\[]"))

     );;cond

    (while (and continue
                (re-search-backward regexp limit t))
      (unless (web-mode-is-comment-or-string)
        (if (string= (string (char-after)) paren)
            (progn 
              (setq n (1+ n))
              (if (> n 0) (setq continue nil)))
          (setq n (1- n))))
      )
    );;let
;;  (message (web-mode-current-trimmed-line))
  )

(defun web-mode-count-char-in-string (char &optional string)
  "Count char in string."
  (let ((i 0) (n 0) l)
    (setq l (length string))
    (while (< i l)
      (if (char-equal (elt string i) char)
          (setq n (1+ n)))
      (setq i (1+ i)))
    n))

(defun web-mode-element-at-point ()
  "Return element at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((continue t)
          cont
          line l1 l2
          (pos (point)))
      (while continue
        (setq l1 (web-mode-current-line-number))
        (end-of-line)
        (setq cont t)
        (while cont
          (re-search-backward "<[[:alpha:]/]" nil t)
          (setq cont (web-mode-is-comment-or-string)))
;;          (setq cont (web-mode-is-csss)))
        (setq cont t)
        (while cont
          (re-search-forward "[[:alnum:] /\"']>" nil t)
          (setq cont (web-mode-is-comment-or-string)))
;;          (setq cont (web-mode-is-csss)))
;;        (message "point=%d" (point))
        (setq l2 (web-mode-current-line-number))
        (if (eq l1 l2) (setq continue nil))
        )
      (end-of-line)
;;      (setq line (buffer-substring-no-properties pos (point)))
      (setq line (buffer-substring pos (point)))
      (setq line (replace-regexp-in-string "[\r\n]" "" line))
      (setq line (replace-regexp-in-string "[ ]+" " " line))
      (message "elt at point: %s" line)
      line
      )))

(defun web-mode-rename-element ()
  "Rename the current HTML element."
  (interactive)
  (save-excursion
    (let (pos tag)
      (setq tag (read-from-minibuffer "Tag name? "))
      (when (and (> (length tag) 0) 
                 (web-mode-beginning-of-element)
                 (looking-at "<\\([[:alpha:]]+\\)"))
        (setq pos (point))
        (if (not (web-mode-is-void-element))
            (save-match-data
              (web-mode-match-tag)
              (if (looking-at "</[ ]*\\([[:alpha:]]+\\)")
                  (replace-match (concat "</" tag))
                )))
        (goto-char pos)
        (replace-match (concat "<" tag))        
        ))))

(defun web-mode-select-element ()
  "Select the current HTML element."
  (interactive)
  (when (or (looking-at-p "<[[:alpha:]]") 
            (and (goto-char (+ 1 (point)))
                 (web-mode-rsb "<[[:alpha:]]")))
    (set-mark (point))
    (web-mode-match-tag)
    (search-forward ">")))

(defun web-mode-delete-element ()
  "Delete the current HTML element"
  (interactive)
  (web-mode-select-element)
  (when mark-active
    (delete-region (region-beginning) (region-end))))

(defun web-mode-duplicate-element ()
  "Duplicate the current HTML element."
  (interactive)
  (let ((offset 0))
    (web-mode-select-element)
    (when mark-active
      (save-excursion 
        (goto-char (region-beginning))
        (setq offset (current-column)))
      (kill-region (region-beginning) (region-end))
      (yank)
      (newline)
      (indent-line-to offset)
      (yank))))

(defun web-mode-is-opened-element (&optional line)
  "Is there any HTML element without a closing tag ?"
  (interactive)
  (let ((deb 0)
        is-closing-tag
        is-void-element
        tag
        n
        ret
        (h (make-hash-table :test 'equal)))
    (unless line (setq line (web-mode-element-at-point)))
;;    (message "line=%s" line)
    (setq line (web-mode-clean-client-line line))
;;    (message "clean-line=%s" line)
    (while (string-match web-mode-tag-regexp line deb)
      (setq deb (match-end 0)
            tag (match-string 1 line)
            is-closing-tag (string= (substring tag 0 1) "/"))
      (if is-closing-tag (setq tag (substring tag 1)))
      (setq n (gethash tag h 0))
      (setq deb (string-match "/?>" line deb))
;;      (setq deb (string-match "[^%?]?>" line deb))
      (setq is-void-element (string= (substring (match-string 0 line) 0 1) "/"))
      (if (or is-void-element (web-mode-is-void-element tag))
          (progn
;;            (message "void tag: %s" tag)
            )
        (if is-closing-tag
            (if (> n 0) (puthash tag (1- n) h))
          (puthash tag (1+ n) h))
        )

      );; while
        
    ;;(message (number-to-string (gethash "strong" h)))
    ;;(message (number-to-string (hash-table-count h)))
    (maphash (lambda (k v) (if (> v 0) (setq ret 't))) h)
;;    (if ret (message "line=%s: opened" line) (message "line=%s: closed" line))
    ret
    )
  )

(defun web-mode-parent-html-element ()
  "Fetch parent element."
  (interactive)
  (let (pos 
        is-closing-tag
        tag
        n
        (continue t)
        (h (make-hash-table :test 'equal)))
    (save-excursion
;;      (unless (string= (string (char-after)) "<")
;;        (progn
      ;;          (forward-char)
;;;;          (search-forward ">") ;; todo : verifier que l'on est pas dans une string
      ;;          (re-search-backward "<[[:alnum:]]+[ ><$]" nil t)))
      (while (and continue
;;                  (re-search-backward "</?[[:alnum:]]+[/ ><$]" nil t))
                  (web-mode-rsb "</?[[:alnum:]]+[/ ><$]"))
;;        (message "ici")
        (forward-char)
;;        (setq is-closing-tag (string= (string (char-after)) "/"))
        (setq is-closing-tag (char-equal (char-after) ?/))
        (if (eq is-closing-tag t) (forward-char))
        (setq nb (skip-chars-forward "[:alnum:]"))
        (setq tag (buffer-substring-no-properties (- (point) nb) (point)))
        (setq n (gethash tag h 0))
;;        (message "%s %d %d" tag n (point))
        (when (not (web-mode-is-void-element tag))
          (search-backward "<")
          (if (eq is-closing-tag t)
              (puthash tag (1- n) h)
            (progn
              (puthash tag (1+ n) h)
              (if (eq n 0)
                  (progn
                    (setq pos (point))
                    (setq continue nil)))
              )
            )
          ) ;; when
        ) ;; while
      ) ;; save-excursion
    (if (null continue) (goto-char pos))
    ) ;; let
  )

(defun web-mode-text-at-point (&optional pos)
  "Text at point."
;;  (unless pos (setq pos (point)))
  (buffer-substring-no-properties (or pos (point)) (line-end-position)))

(defun web-mode-current-trimmed-line ()
  "Line at point, trimmed."
  (web-mode-trim (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position))))

(defun web-mode-trim (string)
  "Remove white spaces in beginning and ending of STRING."
  (replace-regexp-in-string
   "\\`[ \t\n]*" ""
   (replace-regexp-in-string
    "[ \t\n]*\\'" "" string)))

(defun web-mode-is-void-element (&optional tag)
  "Test if tag is a void tag."
  (if tag
      (find (downcase tag) web-mode-void-elements :test 'equal)
    (or (looking-at-p (concat "<\\(" (regexp-opt web-mode-void-elements) "\\)"))
        (looking-at-p "<[^>]+/>")
    )))

(defconst web-mode-void-elements
  '("hr" "br" "col" "input" "link" "meta" "img" 
    "tmpl_var" 
    "h:inputtext"
    "#include" "#assign" "#import" "#else")
  "Void (self-closing) tags.")

(defconst web-mode-php-constants
  (regexp-opt
   (append (if (boundp 'web-mode-extra-php-constants) 
               web-mode-extra-php-constants '())
           '("TRUE" "FALSE" "NULL" "true" "false" "null"
             "STR_PAD_LEFT" "STR_PAD_RIGHT")))
  "PHP constants.")

(defconst web-mode-php-keywords
  (regexp-opt
   (append (if (boundp 'web-mode-extra-php-keywords) 
               web-mode-extra-php-keywords '())
           '("array" "as" "break" "callable" "catch" "class" "const" "continue"
             "default" "die" "do"
             "echo" "else" "elseif" "empty"
             "endfor" "endforeach" "endif" "endswitch" "endwhile" "exit" "extends"
             "for" "foreach" "function"
             "if" "include" "instanceof" "interface" "isset" "list"
             "next" "or" "print" "require" "return" "switch" "try" "unset"
             "var" "when" "while")))
  "PHP keywords.")

(defconst web-mode-php-types
  (eval-when-compile
    (regexp-opt
     '("array" "bool" "boolean" "char" "const" "double" "float"
       "int" "integer" "long" "mixed" "object" "real" "string"
       "Exception")))
  "PHP types.")

(defconst web-mode-css-at-rules
  (eval-when-compile
    (regexp-opt
     '("charset" "import" "media" "page" "font-face" "namespace")))
  "CSS at-rules.")

(defconst web-mode-css-pseudo-classes
  (eval-when-compile
    (regexp-opt
     '("active" "after" "before" "checked" "disabled" "empty" "enabled" 
       "first" "first-child" "first-letter" "first-line" "first-of-type"
       "focus" "hover" "lang" "last-child" "last-of-type" "left" "link"
       "not" "nth-child" "nth-last-child" "nth-of-type"
       "only-child" "only-of-type"
       "right" "root" "selection" "target" "visited")))
  "CSS pseudo-classes (and pseudo-elements).")

(defconst web-mode-jsp-keywords
  (regexp-opt
   (append (if (boundp 'web-mode-extra-jsp-keywords) 
               web-mode-extra-jsp-keywords '())
           '("if" "else" "for" "while" "do" "case" "function"
             "new" "page" "include" "tag" "taglib" "package" "try" "catch"
             "throw" "throws"
             "return"
             "in" "end"
             )))
  "JSP keywords.")

(defconst web-mode-asp-keywords
  (regexp-opt
   (append (if (boundp 'web-mode-extra-asp-keywords) 
               web-mode-extra-asp-keywords '())
           '("case" "catch" "do" "else" "end" "for" "function"
             "if" "in" "include" "new" "package" "page" "return"
             "tag" "throw" "throws"
             "try" "while")))
  "ASP keywords.")

(defconst web-mode-twig-keywords
  (eval-when-compile
    (regexp-opt
     '("as" "autoescape" "block" "do" "embed" "else" "elseif"
       "endautoescape" "endblock" "endembed" "endfilter"
       "endfor" "endif" "endmacro" "endraw" "endsandbox" "endspaceless"
       "extends" "filter" "flush" "for" "from" 
       "if" "ignore" "import" "in" "include" 
       "macro" "missing" "not" "raw"
       "sandbox" "set" "spaceless" "use" "var")))
  "Twig keywords.")


(defconst web-mode-directives
  (eval-when-compile
    (regexp-opt
     '("include" "page" "taglib" 
       "Assembly" "Control" "Implements" "Import" 
       "Master" "OutputCache" "Page" "Reference" "Register")))
  "Directives.")

(defconst web-mode-js-keywords
  (regexp-opt
   (append (if (boundp 'web-mode-extra-js-keywords) 
               web-mode-extra-js-keywords '())
           '("function" "for" "if" "var" "while" "new" "try" "catch" 
             "return" "true" "false"
             )))
  "JavaScript keywords.")

(defun web-mode-highlight-css-props (limit)
  "Highlight css props."
  (let ((font-lock-keywords web-mode-css-props-font-lock-keywords)
        (font-lock-keywords-case-fold-search nil)
        (font-lock-keywords-only t)
        (font-lock-extend-region-functions nil)
        ;;        (limit (point-max))
        open close)
    (setq limit (point-max))
    ;;    (message "limit=%s" limit)
    (when (search-forward "{" limit t)
      (setq open (point))
      (search-forward "}" limit t)
      (setq close (- (point) 1))
      ;;      (if (not (web-mode-is-comment-or-string))
      (font-lock-fontify-region open close)
      ;;        )
      )
    ))

(defun web-mode-highlight-client-blocks (limit)
  "Highlight client blocks."
  (let ((inhibit-modification-hooks t)
        (font-lock-keywords nil)
        (font-lock-keywords-case-fold-search nil)
        (font-lock-keywords-only t)
        (font-lock-extend-region-functions nil)
        open close closing-string chunk pos tag)
    (setq limit (point-max))
    (when (re-search-forward "<\\(style\\|script\\)[^>]*>" limit t)
      (setq open (point)
            tag (match-string 0)
            chunk (substring (match-string 0) 0 3))
      
      (cond
       
       ((string= "<sc" chunk) 
        (setq font-lock-keywords web-mode-script-font-lock-keywords
              closing-string "</script>"))
       
       ((string= "<st" chunk) 
        (setq font-lock-keywords web-mode-style-font-lock-keywords
              closing-string "</style>"))
       
       );;cond

;;      (message "%s - %s" chunk closing-string)

      (when (and closing-string
                 (search-forward closing-string limit t))
        (setq close (match-beginning 0)
              pos (point))
;;        (message "%d %d" open close)
        (font-lock-default-unfontify-region open close)
        (font-lock-fontify-region open close)

        (when (string= "<st" chunk)
;;          (message "%s" chunk)
          (goto-char open)
          (while (and (re-search-forward "{\\([^}]+\\)}" limit t)
                      (< (point) pos))
;;            (message "%s %d %d" (match-string 1) (match-beginning 1) (match-end 1))
            (setq font-lock-keywords web-mode-css-props-font-lock-keywords)
            (font-lock-default-unfontify-region (match-beginning 1) (match-end 1))
            (font-lock-fontify-region (match-beginning 1) (match-end 1))
            );;while
          (goto-char pos)
          )
        pos
        );; when

      );; when

    ))

(defun web-mode-highlight-server-blocks (limit)
  "Highlight server blocks."
  (let ((inhibit-modification-hooks t)
        (font-lock-keywords nil)
        (font-lock-keywords-case-fold-search nil)
        (font-lock-keywords-only t)
        (font-lock-extend-region-functions nil)
        open-out close-out type open close closing-string chunk ms)
;;    (message "%d %d : %s" limit (point-max) (web-mode-current-trimmed-line))
    (setq limit (point-max))
    (when (re-search-forward "\\(<\\?php\\|<\\?=\\|{%\\|<%[@]?\\)" limit t)
;;      (message "beg=%d point=%d" (match-beginning 0) (point))
      (setq open (point)
            open-out (match-beginning 0)
            chunk (substring (match-string 0) 0 2)
            ms (match-string 0))
;;      (message "%s" (match-string 0))
      (cond

       ((string= "<?" chunk) 
        (setq font-lock-keywords web-mode-php-font-lock-keywords
              closing-string "?>"
              type 'php))

   ;; ((string= "<#" ms)
       ;;  (setq font-lock-keywords web-mode-directive-font-lock-keywords
       ;;        closing-string ">"
       ;;        type 'directive))

       ((string= "<%@" ms)
        (setq font-lock-keywords web-mode-directive-font-lock-keywords
              closing-string "%>"
              type 'directive))

       ((string= "{%" ms)
        (setq font-lock-keywords web-mode-twig-font-lock-keywords
              closing-string "%}"
              type 'php))


       ((string= "<%" chunk)
        (cond
         ((string= web-mode-server-language "asp")
          (setq type 'asp
                font-lock-keywords web-mode-asp-font-lock-keywords))
         (t
          (setq type 'jsp
                font-lock-keywords web-mode-jsp-font-lock-keywords))
         )
        (setq closing-string "%>"))

       );;cond
      
      (if (and closing-string
               (search-forward closing-string limit t))
          (progn
            (setq close (match-beginning 0)
                  close-out (match-end 0))
            (font-lock-default-unfontify-region open close)
            (font-lock-fontify-region open close)
            close-out
            )
        (when (string= closing-string "?>")
          (setq close (point-max))
          (font-lock-default-unfontify-region open close)
          (font-lock-fontify-region open close)
          (point-max)
          )
        )
      
      );;when
    
    ))

(defun web-mode-font-lock-extend-css-region ()
  "Extend CSS region."
  (save-excursion
    ;;      (message "beg(%d) end(%d) max(%d)" font-lock-beg font-lock-end (point-max))
    ;;      (setq pt (point))
    ;;      (goto-char pt)
    (when (search-backward "{" nil t)
      (beginning-of-line)
      (setq font-lock-beg (point)))
    ))

(defun web-mode-extend-after-change-region (beg end old-len)
  "web-mode-extend-after-change-region"
  (message "%d %d %d" beg end old-len)
  (cons (point-min) (point-max))
;;  (cons 13 340)
  )

(defun web-mode-font-lock-extend-region ()
  "Extend font lock region."
  ;;(web-mode-scan-init)
;;  (message "point=(%d) font-lock-beg=(%d) font-lock-end=(%d) %S %S %S" (point) font-lock-beg font-lock-end (get-text-property (point) 'block-language) (get-text-property (point) 'block-side) (get-text-property (point) 'block-type))
  (save-excursion
    (let ((pos (point)))

      (goto-char font-lock-beg)

      (cond

       ((eq (get-text-property pos 'block-side) 'server)
        (setq font-lock-beg 1))
       
       ((or (looking-at-p "[ \t]*<[[:alpha:]%?]")
            (re-search-backward "^[ \t]*<[[:alpha:]]" nil t))
        (beginning-of-line)
        (setq font-lock-beg (if (web-mode-is-comment) 1 (point))))

       (t
        (setq font-lock-beg 1))

       )
      
      (setq font-lock-end (point-max))

      )))

;; Unified Expression Language
(defconst web-mode-html-font-lock-keywords
  (list
   '("</?[[:alpha:]][[:alnum:]:_]*?\\(>\\|<\\|[ ]\\|/\\|$\\)"
     0 'web-mode-html-tag-face t t)
   '("\\(</?\\|/?>\\)"
     0 'web-mode-html-tag-face t t)
   '("[[:space:]>]\\([[:alnum:]_-]+=\\)\\(\"[^\"]*\"\\)?" 
     (1 'web-mode-html-attr-name-face t t)
     (2 'web-mode-html-attr-value-face t t))
   '("\\([$#{]{\\)\\([^}]+\\)\\(}}?\\)" 
     (1 'web-mode-preprocessor-face t t)
     (2 'web-mode-variable-name-face t t)
     (3 'web-mode-preprocessor-face t t))
   '(web-mode-highlight-client-blocks)
   '(web-mode-highlight-server-blocks)
   '("\\(<\\?=\\|<\\?php\\|<\\?\\|<%[!$@=#:]?\\|{%\\|%}\\|[?%]>\\)" 0 
     'web-mode-preprocessor-face t t)
   '("^<\\(!D\\|\\?x\\)[^>]*>" 0 'web-mode-doctype-face t t)
   '("<[!#]--\\(.\\|\n\\)*?-->" 0 'web-mode-comment-face t t)
   '("<%--\\(.\\|\n\\)*?%>" 0 'web-mode-comment-face t t)
   '("{#\\(.\\|\n\\)*?#}" 0 'web-mode-comment-face t t)
   '("</?\\([#@][[:alpha:]._]+\\|[[:alpha:]]+[:_][[:alpha:]]+\\)"
     (1 'web-mode-preprocessor-face t t))
   ))

(defconst web-mode-style-font-lock-keywords
  (list
   '("[^][#*~)(>,+{}@.:]" 0 'web-mode-css-rule-face)
   (cons (concat ":\\(" web-mode-css-pseudo-classes "\\)\\>") 
         '(1 'web-mode-css-pseudo-class-face t t))
   (cons (concat "@\\(" web-mode-css-at-rules "\\)\\>") 
         '(1 'web-mode-css-at-rule-face t t))   
   '("\\[\\(.+\\)\\]" (1 nil t t))
   '("(\\(.+\\))" (1 nil t t))
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
   '("/\\*\\(.\\|\n\\)*?\\*/" 0 'web-mode-comment-face t t)
   ))

(defconst web-mode-directive-font-lock-keywords
  (list
   (cons (concat "\\(" web-mode-directives "\\)[ ]+") 
         '(1 'web-mode-keyword-face t t))
   '("[[:space:]^]\\([[:alpha:]]+=\\)\\(\"[^\"]*\"\\)" 
     (1 'web-mode-html-attr-name-face t t)
     (2 'web-mode-html-attr-value-face t t))
   ))

(defconst web-mode-twig-font-lock-keywords
  (list
   (cons (concat "\\(" web-mode-twig-keywords "\\)[ ]+") 
         '(1 'web-mode-keyword-face t t))
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-name-face)
   ))

(defconst web-mode-css-font-lock-keywords
;;  (append (cdr web-mode-style-font-lock-keywords) 
;;          '(web-mode-highlight-css-props))
  (list
   '("[^][#*~)(>,+{}@.:]" 0 'web-mode-css-rule-face)
   (cons (concat ":\\(" web-mode-css-pseudo-classes "\\)\\>") 
         '(1 'web-mode-css-pseudo-class-face t t))
   (cons (concat "@\\(" web-mode-css-at-rules "\\)\\>") 
         '(1 'web-mode-css-at-rule-face t t))   
   '("\\[\\(.+\\)\\]" (1 nil t t))
   '("(\\(.+\\))" (1 nil t t))
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
   '(web-mode-highlight-css-props)
   '("/\\*\\(.\\|\n\\)*?\\*/" 0 'web-mode-comment-face t t)
   )
   
;;  (cdr web-mode-style-font-lock-keywords)
  )

(defconst web-mode-css-props-font-lock-keywords
  (list
   '("[[:alpha:]-]\\{3,\\}[ ]?:" 0 'web-mode-css-prop-face)
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
   '("![ ]?important" 0 font-lock-builtin-face t t)
   '("#[[:alnum:]]\\{3,6\\}" 0 font-lock-builtin-face t t)
   '("/\\*\\(.\\|\n\\)*?\\*/" 0 'web-mode-comment-face t t)
   ))

(defconst web-mode-script-font-lock-keywords
  (list
   '("\\<\\([[:alnum:]_.]+\\)[ ]?(" 1 'web-mode-function-name-face)
   (cons (concat "\\<\\(" web-mode-js-keywords "\\)\\>") 
         '(0 'web-mode-keyword-face))
   '("\\([[:alnum:]]+\\):" 1 'web-mode-variable-name-face)
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
   '("[^:\"]\\(//.+\\)" 1 'web-mode-comment-face t t)
   '("/\\*\\(.\\|\n\\)*?\\*/" 0 'web-mode-comment-face t t)
   ))

(defconst web-mode-asp-font-lock-keywords
  (list
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("\\<\\([[:alnum:].]+\\)[ ]+[[:alpha:]]+" 1 'web-mode-type-face)
   (cons (concat "\\<\\(" web-mode-asp-keywords "\\)\\>") 
         '(0 'web-mode-keyword-face t t))
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
   '("[^:\"]\\(//.+\\)" 1 'web-mode-comment-face t t)
   ))

(defconst web-mode-jsp-font-lock-keywords
  (list
   '("\\<\\([[:alnum:]._]+\\)[ ]?(" 1 'web-mode-function-name-face)
   '("@\\(\\sw*\\)" 1 'web-mode-variable-name-face t t)
   '("\\<\\([[:alnum:].]+\\)[ ]+[[:alpha:]]+" 1 'web-mode-type-face)
   (cons (concat "\\<\\(" web-mode-jsp-keywords "\\)\\>") 
         '(0 'web-mode-keyword-face t t))
   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)
   '("[^:\"]\\(//.+\\)" 1 'web-mode-comment-face t t)
   '("/\\*\\(.\\|\n\\)*?\\*/" 0 'web-mode-comment-face t t)
   ))

(defconst web-mode-php-font-lock-keywords
  (list

   (cons (concat "\\<\\(" web-mode-php-keywords "\\)\\>") 
         '(0 'web-mode-keyword-face))

   (cons (concat "(\\<\\(" web-mode-php-types "\\)\\>") 
         '(1 'web-mode-type-face))

   (cons (concat "\\<\\(" web-mode-php-constants "\\)\\>") 
         '(0 'web-mode-constant-face))

   '("\\<\\(\\sw+\\)[ ]?(" 1 'web-mode-function-name-face)

   '("[[:alnum:]_][ ]?::[ ]?\\(\\sw+\\)" 1 'web-mode-constant-face)

   '("->[ ]?\\(\\sw+\\)" 1 'web-mode-variable-name-face)

   '("\\<\\(\\sw+\\)[ ]?::" 1 'web-mode-type-face)

   '("\\<\\(instanceof\\|class\\|new\\)[ ]+\\([[:alnum:]_]+\\)" 2 'web-mode-type-face)

   '("\\<\\([$]\\)\\(\\sw*\\)"
     (1 nil t t)
     (2 'web-mode-variable-name-face t t))

   '("\\(\"[^\"]*\"\\|'[^']*'\\)" 0 'web-mode-string-face t t)

   '("/\\*\\(.\\|\n\\)*?\\*/" 0 'web-mode-comment-face t t)
   '("[^:\"]\\(//.+\\)" 1 'web-mode-comment-face t t)

   ))

(defvar web-mode-snippets
  (list
   '("table" 
     "<table><tbody>\n<tr>\n<td>"
     "</td>\n<td></td>\n</tr>\n</tbody></table>")
   '("ul" 
     "<ul>\n<li>" 
     "</li>\n<li></li>\n</ul>")
   '("if" 
     "<?php if ( as ): ?>\n" 
     "\n<?php endif; ?>")
   '("for" 
     "<?php for ( ; ; ): ?>\n" 
     "\n<?php endfor; ?>")
   '("foreach" 
     "<?php foreach ( as ): ?>\n" 
     "\n<?php endforeach; ?>")
   '("doctype" 
     "<!DOCTYPE html>\n")
   '("html5"
     "<!DOCTYPE html>\n<html>\n<head>\n<title></title>\n<meta charset=\"utf-8\" />\n</head>\n<body>\n" 
     "\n</body>\n</html>")
   )
  "Code snippets")

(defun web-mode-insert (code)
  "Prompt for snippet code."
  (interactive 
   (list (completing-read
          "Snippet: " 
          (web-mode-snippet-codes))))
  (web-mode-insert-snippet code))

(defun web-mode-snippet-codes ()
  "Snippet codes."
  (interactive)
  (let (codes 
        (counter 0) 
        snippet 
        (l (length web-mode-snippets)))
    (while (< counter l)
      (setq snippet (nth counter web-mode-snippets))
      (setq counter (1+ counter))
      (add-to-list 'codes (list (nth 0 snippet) counter)))
;;    (message "%S" codes)
    codes))

(defun web-mode-toggle-folding ()
  "Toggle folding on a block."
  (interactive)
  (with-silent-modifications
    (save-excursion
      (let (beg-outside end-outside beg-inside end-inside overlay overlays pos)
        (back-to-indentation)
        (setq overlays (overlays-at (point)))
        (if overlays
            (progn 
              (setq overlay (car overlays))
              (setq beg-inside (overlay-start overlay)
                    end-inside (overlay-end overlay))
              (remove-overlays beg-inside end-inside)
              (put-text-property beg-inside end-inside 'invisible nil))
          (when (and (char-equal (char-after) ?<)
                     (or (and (looking-at-p "<[[:alpha:]]")
                              (not (web-mode-is-void-element)))
                         (looking-at-p "<\\?php[ ]+\\(if\\|while\\|for\\)")))
            (setq beg-outside (point))
            (if (looking-at-p "<\\?")
                (web-mode-rsf "\\?>")
              (web-mode-rsf ">"))
            (setq beg-inside (point))
            (goto-char beg-outside)
            (web-mode-match-tag)
            (setq end-inside (point))
            (if (looking-at-p "<\\?")
                (web-mode-rsf "\\?>")
              (web-mode-rsf ">"))
            (setq end-outside (point))
            ;;          (message "beg-out(%d) beg-in(%d) end-in(%d) end-out(%d)" beg-outside beg-inside end-inside end-outside)
            (setq overlay (make-overlay beg-outside end-outside))
            (overlay-put overlay 'face 'web-mode-folded-face)
            (put-text-property beg-inside end-inside 'invisible t)
            ))))))

(defun web-mode-goto-block-beg (&optional pos)
  "Block type beg"
  (interactive)
  (unless pos (setq pos (point)))
  (unless (bobp)
    (when (string= (get-text-property pos 'block-language)
                   (get-text-property (- pos 1) 'block-language))
      (setq pos (previous-single-property-change pos 'block-language))
      (goto-char pos))
    );;unless
  t)

(defun web-mode-toggle-comments ()
  "Toggle comments visbility"
  (interactive)
  (save-excursion
    (if web-mode-comments-invisible
        (remove-overlays))
    (setq web-mode-comments-invisible (null web-mode-comments-invisible))
    (let ((continue t) 
          (pos (point-min)) 
          (visibility web-mode-comments-invisible)
          overlay end)
      (while continue
        (setq pos (next-single-property-change pos 'face))
        (if (null pos)
            (setq continue nil)
          (when (eq (get-text-property pos 'face) 'web-mode-comment-face)
            (setq end (next-single-property-change pos 'face))
            (put-text-property pos end 'invisible visibility)
            (when visibility
              (setq overlay (make-overlay pos end)))
            (goto-char pos)
            )
          )
        )
      );;let
    )
  )

(defun web-mode-line-type (&optional pos)
  "Line type."
  (save-excursion
    (let (type)
      (if pos (goto-char pos))
      
      (back-to-indentation)
      (cond
       
       ((web-mode-in-server-block 'php)
        (setq type "php"))

       ((web-mode-in-server-block 'jsp)
        (setq type "java"))
       
       ((web-mode-in-server-block 'directive)
        (setq type "html"))
       
       ((web-mode-in-server-block 'asp)
        (setq type "asp"))
       
       ((web-mode-in-client-block 'js)
        (setq type "script"))
       
       ((web-mode-in-client-block 'css)
        (setq type "style"))
              
       (t
        (setq type "html"))
       
       );; cond
      
      type
        
      )))
  
(defun web-mode-comment-uncomment (&optional pos)
  "Comment or uncomment line(s) at point."
  (interactive)
  (unless pos (setq pos (point)))
  (if (web-mode-is-comment)
      (web-mode-uncomment pos)
    (web-mode-comment pos))
  (web-mode-scan (point-min) (point-max)))

(defun web-mode-comment (&optional pos)
  "Comment line(s) at point."
  (interactive)
  (unless pos (setq pos (point)))
  (save-excursion
    (let (type sel beg end)
      
      (if mark-active
          (progn
            (setq beg (region-beginning) 
                  end (region-end))
            (setq type (web-mode-line-type beg))
;;            (message "(%d)->(%d)" (region-beginning) (region-end))
            )
        (setq type (web-mode-line-type (line-beginning-position)))
;;        (message "type=%S" type)
        (if (string= type "html")
            (progn
              (back-to-indentation)
              (web-mode-select-element))
          (end-of-line)
          (set-mark (line-beginning-position))
          );;if
        (setq beg (region-beginning) 
              end (region-end))
        );; if
      
;;      (message "type=%s" type)
      
      (setq sel (web-mode-trim (buffer-substring-no-properties beg end)))
;;      (message "[type=%s] sel=%s" type sel)
      (delete-region beg end)
      (deactivate-mark)

      (cond
       
       ((string= type "html")

        (web-mode-insert-and-indent (concat "<!-- " sel " -->"))
        )
       
       ((or (string= type "php") (string= type "script") (string= type "style"))
        (web-mode-insert-and-indent (concat "/* " sel " */"))
        )
       
       (t
        (web-mode-insert-and-indent (concat "/* " sel " */")))
       
       ))))

(defun web-mode-uncomment (&optional pos)
  "Uncomment line(s) at point."
  (interactive)
;;  (web-mode-scan-init)
  (unless pos (setq pos (point)))
  (let ((beg pos) 
        (end pos)
        (sub2 "")
        comment prop)

    (if (eq (get-text-property pos 'server-type) 'comment)
        (setq prop 'server-type)
      (setq prop 'client-type))

    (if (not (bobp))
        (setq beg (or (previous-single-property-change pos prop) pos)))
    
    (if (not (eobp))
        (setq end (or (next-single-property-change pos prop) pos)))

    (when (> (- end beg) 3)

      (setq comment (buffer-substring-no-properties beg end))
      ;;    (message "beg(%d) end(%d) content[%s]" beg end comment)
    
      (setq sub2 (substring comment 0 2))
      
      (cond
       
       ((member sub2 '("<!" "<%"))
        (setq comment (replace-regexp-in-string "\\(^<[!%]--[ ]?\\|[ ]?--[%]?>$\\)" "" comment)))
       
       ((string= sub2 "{#")
        (setq comment (replace-regexp-in-string "\\(^{#[ ]?\\|[ ]?#}$\\)" "" comment)))
       
       ((string= sub2 "/*")
        (setq comment (replace-regexp-in-string "\\(^/\\*[ ]?\\|[ ]?\\*/$\\)" "" comment)))
       
       ((string= sub2 "//")
        (setq comment (replace-regexp-in-string "\\(^//\\)" "" comment)))
       
       )
      
      ;;    (message "after[%s]" comment)
      
      (delete-region beg end)
      (web-mode-insert-and-indent comment)
      (goto-char beg)
      (back-to-indentation)

      );;when

    ))

(defun web-mode-insert-snippet (code)
  "Insert snippet."
  (let (beg 
        (continue t) 
        (counter 0) 
        end 
        sel 
        snippet 
        (l (length web-mode-snippets)) 
        pos)
    (when mark-active
      (setq sel (web-mode-trim
                 (buffer-substring-no-properties 
                  (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end)))
    (while (and continue (< counter l))
      (setq snippet (nth counter web-mode-snippets))
      (when (string= (nth 0 snippet) code)
        (setq continue nil))
      (setq counter (1+ counter)))
    (when (and (null continue)
               (nth 1 snippet))
      (setq beg (point-at-bol))
      (insert (nth 1 snippet))
      (setq pos (point))
      (when sel 
        (insert sel)
        (setq pos (point)))
      (if (nth 2 snippet) (insert (nth 2 snippet)))
      (setq end (point-at-eol))
      (goto-char pos)
      (indent-region beg end))
    ))

(defun web-mode-insert-and-indent (text)
  "Insert and indent text."
  (interactive)
  (let (beg end)
    (setq beg (point-at-bol))
    (insert text)
    (setq end (point-at-eol))
    (indent-region beg end)))

(defun web-mode-match-tag ()
  "Match tag."
  (interactive)
  (let (pos)

    (when (> (current-indentation) (current-column))
      (back-to-indentation))

    (setq pos (point))
    
    (cond 
     
     ((web-mode-is-comment-or-string)
      )
     
     ((and (eq (get-text-property pos 'block-language) 'php)
           (web-mode-goto-block-beg)
           (looking-at-p "<\\?php[ ]+\\(end\\)?\\(for\\|if\\|else\\|while\\)"))
      (web-mode-match-php-tag))

     ((and (search-forward ">")
           (web-mode-rsb web-mode-tag-regexp nil t))
      (web-mode-match-html-tag))

     );; cond

    ))

(defun web-mode-match-html-tag (&optional pos)
  "Match HTML tag."
  (unless pos (setq pos (point)))
  (let (closing-tag nb tag)
    (forward-char)
;;    (setq closing-tag (string= (string (char-after)) "/"))
    (setq closing-tag (char-equal (char-after) ?/))
    (if (eq closing-tag t)
        (forward-char))
    (setq nb (skip-chars-forward "a-z:A-Z0-9@"))
    (setq tag (buffer-substring-no-properties (- (point) nb) (point)))
;;    (message "tag=%s" tag)
    (if (web-mode-is-void-element tag)
        (message "void tag")
      (if (eq closing-tag t)
          (web-mode-match-html-opening-tag tag pos)
        (web-mode-match-html-closing-tag tag pos)))))

(defun web-mode-match-html-closing-tag (tag pos)
  "Match closing HTML closing tag."
  (let (counter n regexp)
    (setq counter 1)
    (setq n 0)
    (search-forward ">")
    (setq regexp (concat "</?" tag))
    (while (and (> counter 0)
                (re-search-forward regexp nil t))
      ;;      (when (not (web-mode-is-comment-or-string))
      (when (not (web-mode-is-csss))
        (setq n (1+ n))
        ;; (message "[%s] point=%d line=%d" 
        ;;          (match-string-no-properties 0)
        ;;          (point)
        ;;          (web-mode-current-line-number))
        (if (string= (substring (match-string-no-properties 0) 0 2) "</")
;;        (if (eq (length (match-string-no-properties 0)) 2)
            (setq counter (- counter 1))
          (setq counter (+ counter 1))))
      )
    (if (> n 0)
        (search-backward "<" 1 t)
      (goto-char pos))
    ))

(defun web-mode-match-html-opening-tag (tag pos)
  "Match opening HTML tag."
  (let (counter n regexp)
    (setq counter 1)
    (setq n 0)
    (search-backward "<")
    (setq regexp (concat "</?" tag))
    (while (and (> counter 0)
                (re-search-backward regexp nil t))
      (when (not (web-mode-is-comment-or-string))
        (setq n (1+ n))
        (if (string= (substring (match-string-no-properties 0) 0 2) "</")
            (setq counter (+ counter 1))
          (setq counter (- counter 1))))
      )
    (if (> n 0)
        ()
;;       (search-backward "<" 1 t)
      (goto-char pos))
    ))

(defun web-mode-match-php-tag ()
  "Match PHP tag."
  (let (beg end code regexp type)
    (forward-char)
    (setq beg (+ (point) 4))
    (search-forward ">")
    (setq end (- (point) 2))
    (setq code (buffer-substring-no-properties beg end))
    (if (string-match-p "for" code)
        (if (string-match-p "foreach" code)
            (setq regexp "<\\?php[ ]+\\(foreach\\|endforeach\\)"
                  type   "foreach")
          (setq regexp "<\\?php[ ]+\\(for\\|endfor\\)"
                type   "for"))
      (setq regexp "<\\?php[ ]+\\(if\\|else\\|elseif\\|endif\\)"
            type   "if"))
    (if (string-match-p " end" code)
        (web-mode-match-opening-php-tag regexp type)
      (web-mode-match-closing-php-tag regexp type))))

(defun web-mode-match-opening-php-tag (regexp type)
  "Match PHP opening tag."
  (let ((counter 1) match)
    (search-backward "<")
    (while (and (> counter 0)
                (re-search-backward regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match-p "[ ]+\\(if\\|for\\)" match)
          (setq counter (1- counter))
        (if (string-match-p "[ ]+end\\(if\\|for\\)" match)
            (setq counter (1+ counter)))
        );; if
;;      (message "%s %d" (web-mode-current-trimmed-line) counter)
      );; while
    ))

(defun web-mode-match-closing-php-tag (regexp type)
  "Match PHP closing tag."
  (let ((counter 1) 
        match)
    (while (and (> counter 0)
                (re-search-forward regexp nil t))
      (setq match (match-string-no-properties 0))
      (if (string-match-p "<\\?php \\(if\\|for\\)" match)
          (setq counter (1+ counter))
        (unless (and (> counter 1)
                     (string-match-p "else" match))
          (setq counter (1- counter)))
        ))
    (search-backward "<")))

(defun web-mode-debug-point ()
  (interactive)
  (what-cursor-position))

(defvar web-mode-autocompletes
  (list
   '("<?p" "hp  ?>" "\\?>" 3)
   '("<?=" "?>" "\\?>" 0)
   '("<!-" "-  -->" "--" 2))
  "Autocompletes")

(defun web-mode-on-after-change2 (beg end len)
  "Autocomplete"
  (message "beg=%d, end=%d, len=%d, cur=%d" beg end len (current-column))
  (backtrace)
  )

(defun web-mode-on-after-change (beg end len)
  "Autocomplete"
;;  (message "beg=%d, end=%d, len=%d, cur=%d" beg end len (current-column))

  (let ((chunk "") 
        (pos (point))
        (cur-col (current-column))
        tag
        continue
        found
        (i 0)
        counter
        expr
        (l (length web-mode-autocompletes))
        pos-end
        after
        scan-beg scan-end
        c)

    (when (and web-mode-autocompletes-flag
;;               (not (= len (- end beg)))
               (= len 0)
               (= 1 (- end beg)))

      (if (> (+ end 10) (line-end-position))
          (setq pos-end (line-end-position))
        (setq pos-end (+ end 10)))
      (setq after (buffer-substring-no-properties end pos-end))
      (when (and (not found)
                 (>= cur-col 2)
                 (string= "</" (buffer-substring-no-properties (- beg 1) end)))
        (setq continue t
              counter 1)
        (while (and continue 
                    (re-search-backward web-mode-tag-regexp 0 t))
          (when (not (web-mode-is-comment-or-string))
            (setq tag (substring (match-string-no-properties 0) 1))
;;            (message "tag=%S" tag)
            (if (string= (substring tag 0 1) "/")
                (setq counter (1+ counter))
              (if (not (web-mode-is-void-element))
                  (setq counter (1- counter))
                (message "tag %s is void" tag)
                ))
            (if (eq counter 0)
                (progn
                  (setq continue nil
                        found t)
                  (goto-char pos)
                  (if (looking-at ">")
                      (progn
                        (insert tag)
                        (goto-char (+ (point) 1)))
                    (insert (concat tag ">")))
                  ))
            ) ;; when
          ) ;; while
        (if continue (goto-char pos))
        
        );; when

      (when (and (>= cur-col 3)
                 (not found))
        (setq chunk (buffer-substring-no-properties (- beg 2) end))

        (while (and (< i l)
                    (not found))
          (setq expr (elt web-mode-autocompletes i))
;;          (message "%S" expr)
          (if (string= (elt expr 0) chunk)
              (unless (string-match-p (elt expr 2) after)
                (insert (elt expr 1))
                (goto-char (+ pos (elt expr 3)))
                (setq found t)))
          (setq i (1+ i)))        
        ) ;; when
      
      )

    (save-excursion
      (when (not (= len (- end beg)))
        (setq scan-end (point-max))
        (cond
         ((or (> (- end beg) 1) (> len 1))
          (setq scan-beg 1))
         ((web-mode-rsb-html "<[[:alpha:]]")
          (setq scan-beg (point)))
         (t
          (setq scan-beg 1))
         );;cond
        (web-mode-scan scan-beg scan-end)
        ))
    
    ))

(defun web-mode-reload ()
  "Reload web-mode."
  (interactive)
  (unload-feature 'web-mode)
  (web-mode)
  (if (fboundp 'web-mode-hook)
      (web-mode-hook)))

(provide 'web-mode)

;;; web-mode.el ends here





;; (defun web-mode-highlight-style-block (limit)
;;   "Highlight a <style> bloc."
;;   (let ((font-lock-keywords web-mode-style-font-lock-keywords)
;;         (font-lock-keywords-case-fold-search nil)
;;         (font-lock-keywords-only t)
;;         (font-lock-extend-region-functions nil)
;;         (limit (point-max))
;;         open close)
    
;;     (when (re-search-forward "<style[^>]*>" limit t)
;;       (setq open (point))
;;       (when (search-forward "</style>" limit t)
;;         (setq close (match-beginning 0))
;;         (font-lock-fontify-region open close)
;;         ))

;;     ;; (when (re-search-forward
;;     ;;        "<style>\\(\\(:.\\|\n\\)+\\)</style>"
;;     ;;        limit t)
;;     ;;   (message "%d %d" (match-beginning 1) (match-end 1))
;;     ;;   (font-lock-fontify-region (match-beginning 0) (match-end 0)))
    
;;     ))

;; (defun web-mode-highlight-script-block (limit)
;;   "Highlight a <script> bloc."
;;   (let ((font-lock-keywords web-mode-script-font-lock-keywords)
;;         (font-lock-keywords-case-fold-search nil)
;;         (font-lock-keywords-only t)
;;         (font-lock-extend-region-functions nil)
;;         (limit (point-max))
;;         open close)

;;     (when (re-search-forward "<script[^>]*>" limit t)
;;       (setq open (point))
;;       (when (search-forward "</script>" limit t)
;;         (setq close (match-beginning 0))
;; ;;        (message "%d %d" open close)
;;         (font-lock-fontify-region open close)
;;         )
;;       )
    
;;     ;; (when (re-search-forward
;;     ;;        "<script.*?>\\(.\\|\n\\)*?</script>"
;;     ;;        limit t)
;;     ;;   (font-lock-fontify-region (match-beginning 0) (match-end 0)))
    
;; ))

;; (defun web-mode-highlight-php-block (limit)
;;   "Highlight a PHP bloc."
;;   (let ((font-lock-keywords web-mode-php-font-lock-keywords)
;;         (font-lock-keywords-case-fold-search nil)
;;         (font-lock-keywords-only t)
;;         (font-lock-extend-region-functions nil)
;;         (limit (point-max))
;;         open close)

;;     (when (re-search-forward "\\(<\\?php\\|<\\?=\\)" limit t)
;;       (setq open (point))
;;       (when (search-forward "?>" limit t)
;;         (setq close (match-beginning 0))
;;         (font-lock-fontify-region open close)
;;         ))

    
;;     ;; (when (re-search-forward
;;     ;;        "\\(<\\?php\\|<\\?=\\)\\(.\\|\n\\)*?\\?>"
;;     ;;        limit t)
;;     ;;   (font-lock-fontify-region (match-beginning 0) (match-end 0)))

;;     ))

;; (defun web-mode-highlight-jsp-block (limit)
;;   "Highlight a JSP bloc."
;;   (let ((font-lock-keywords web-mode-jsp-font-lock-keywords)
;;         (font-lock-keywords-case-fold-search nil)
;;         (font-lock-keywords-only t)
;;         (font-lock-extend-region-functions nil)
;;         (limit (point-max))
;;         open close)

;;     (when (re-search-forward "<%[ \n!@=]" limit t)
;;       (setq open (point))
;;       (when (search-forward "%>" limit t)
;;         (setq close (match-beginning 0))
;;         (font-lock-fontify-region open close)
;;         ))

;; ;;    (when (re-search-forward "<%\\(.\\|\n\\)*?%>" limit t)
;; ;;      (font-lock-fontify-region (match-beginning 0) (match-end 0)))

;;     ))



;;  (set (make-local-variable 'font-lock-defaults) '(web-mode-font-lock-keywords))
;;  (set (make-local-variable 'font-lock-keywords-only) t)
;;  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
;;  (set (make-local-variable 'font-lock-syntax-table) nil)
;;  (set (make-local-variable 'font-lock-beginning-of-syntax-function) nil)

  
;; (defun web-mode-in-php-block2 ()
;;   "Detect if point is in a PHP block."
;;   (let (line-current line-open line-close)
;;     (save-excursion
;; ;;      (message "current(%d)" (web-mode-current-line-number))
;; ;;      (end-of-line)
;;       (setq line-current (web-mode-current-line-number))
;;       (and (re-search-backward "<\\?[p=]" nil t)
;;            (setq web-mode-block-beg (+ (point) 
;;                                          (if (char-equal (char-after (+ (point) 2)) ?p) 5 3)
;;                                          ))
;;            (setq line-open (web-mode-current-line-number))
;;            (if (search-forward "?>" nil t) t (goto-char (point-max)))
;; ;;           (progn (message "ici") 't)
;;            (setq line-close (web-mode-current-line-number))
;; ;;           (progn (message "current(%d) from(%d) to(%d)" line-current line-open line-close) 't)
;;            (not (eq line-open line-close))
;;            (>= line-close line-current)
;;            ))))


  
  ;; (define-key web-mode-map [menu-bar]
  ;;   (make-sparse-keymap))
  ;; (define-key web-mode-map [menu-bar web]
  ;;   (cons "Web" (make-sparse-keymap)))
  ;; (define-key web-mode-map [menu-bar web html]
  ;;   (cons "HTML" (make-sparse-keymap)))
  ;; (define-key web-mode-map [menu-bar web html insert-table]
  ;;   '("Insert TABLE" . web-mode-insert-table))
  ;; (define-key web-mode-map [menu-bar web sep1]
  ;;   '("--"))
  ;; (define-key web-mode-map [menu-bar web debug]
  ;;   '("debug" . web-mode-debug-point))

;; (defun web-mode-in-html-block (type)
;;   "Detect if point is in a block"
;;   (let ((pt (point))
;;         (line-current (web-mode-current-line-number))
;;         line-open line-close)
;;     (save-excursion
;;       (and (search-backward (concat "<" type) 0 t)
;;            (search-forward ">")
;;            (setq web-mode-block-beg (point))
;;            (setq line-open (web-mode-current-line-number))
;;            (search-forward (concat "</" type ">") nil t)
;;            (< pt (point))
;;            (setq line-close (web-mode-current-line-number))
;;            (not (eq line-open line-close))
;;            (<= line-current line-close)
;;            (progn (goto-char pt) (beginning-of-line) t)
;;            ;;           (message "-> %s" type)
;;            (not (looking-at (concat "[ \t]*</" type)))
;;            ))
;;     ))



;; (defun web-mode-in-jsp-block ()
;;   "Detect if point is in a JSP block."
;;   (let (line-current line-open line-close)
;;     (save-excursion
;;       (setq line-current (web-mode-current-line-number))
;;       (and (search-backward "<%" nil t)
;;            (setq line-open (web-mode-current-line-number))
;;            (search-forward "%>" nil t)
;;            (setq line-close (web-mode-current-line-number))
;;            (not (eq line-open line-close))
;;            (>= line-close line-current)
;;            ))))


;; (defun web-mode-in-php-block ()
;;   "Detect if point is in a PHP block."
;;   ;;  (let ((pos (line-beginning-position)))
;;   (let ((pos (point)))
;; ;;    (and (string= "php" (get-text-property pos 'block-kind))
;;     (and (eq (get-text-property pos 'block-language) 'php)
;;          (progn
;; ;;           (setq web-mode-block-beg (previous-single-property-change pos 'block-kind))
;;            (setq web-mode-block-beg (previous-single-property-change pos 'block-language))
;;            t))
;;     ))
  
;; (defun web-mode-in-jsp-block ()
;;   "Detect if point is in a JSP block."
;; ;;  (let ((pos (line-beginning-position)))
;;   (let ((pos (point)))
;;     (save-excursion
;;       (and (eq (get-text-property pos 'block-language) 'jsp)
;;            (progn
;;              (setq web-mode-block-beg (previous-single-property-change pos 'block-language))
;;              t)
;;            )
;;       )))

;; (defun web-mode-in-directive-block ()
;;   "Detect if point is in a DIRECTIVE block."
;; ;;  (let ((pos (line-beginning-position)))
;;   (let ((pos (point)))
;;     (save-excursion
;;       (and (eq (get-text-property pos 'block-language) 'directive)
;;            (progn
;;              (setq web-mode-block-beg (previous-single-property-change pos 'block-language))
;;              t)
;;            )
;;       )))

;; (defun web-mode-in-asp-block ()
;;   "Detect if point is in a ASP block."
;; ;;  (let ((pos (line-beginning-position)))
;;   (let ((pos (point)))
;;     (save-excursion
;;       (and (eq (get-text-property pos 'block-language) 'asp)
;;            (progn
;;              (setq web-mode-block-beg (previous-single-property-change pos 'block-language))
;;              t)
;;            )
;;       )))


;; (defun web-mode-sf (expr &optional limit noerror)
;;   "search-forward not in comment or string."
;;   (unless limit (setq limit nil))
;;   (unless noerror (setq noerror t))
;;   (let ((continue t) ret)
;;     (while continue
;;       (setq ret (search-forward expr limit noerror))
;;       (if (or (null ret)
;;               (not (web-mode-is-comment-or-string)))
;;           (setq continue nil)))
;;     ret)
;;   )
