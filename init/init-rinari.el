(require 'find-func)
(defun directory-of-library (library-name)
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

(eval-after-load "rinari"
  `(let ((rinari-lib-dir (directory-of-library "rinari")))
     (unless (require 'jump nil t)
       (error "jump.el not found; please run 'git submodule update --init' in %s"
              rinari-lib-dir))

     ;; Prevent rinari from shadowing ruby-mode and inf-ruby with its bundled copies
     (setq load-path
           (remove (file-name-as-directory (expand-file-name "util/inf-ruby" rinari-lib-dir))
                   (remove (file-name-as-directory (expand-file-name "util" rinari-lib-dir))
                           load-path)))))


(provide 'init-rinari)
