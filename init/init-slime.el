(defvar *cvs-slime-loaded* nil)
(defun sbcl-jack-in ()
  (interactive)
  (when (not *cvs-slime-loaded*)
    (load (expand-file-name "~/quicklisp/slime-helper.el"))
    (slime)
    (setq *cvs-slime-loaded* t)))

(setq inferior-lisp-program "sbcl")

(provide 'init-slime)
