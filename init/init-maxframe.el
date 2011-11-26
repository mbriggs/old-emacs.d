(require 'maxframe)
(if (eq system-type 'darwin)
    (setq mf-offset-x 75))


(defun maybe-maximize-frame ()
    (if window-system (maximize-frame)))

(provide 'init-maxframe)
