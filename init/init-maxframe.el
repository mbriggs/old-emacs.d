(require 'maxframe)

(setq mf-max-width 1920)
(setq mf-offset-x 75)


(defun maybe-maximize-frame ()
    (if window-system (maximize-frame)))

(provide 'init-maxframe)
