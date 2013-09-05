;; Use C-f during file selection to switch to regular find-file
(ido-mode t)  ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations '("\n   " "" "\n   " "\n   ..." "{" "}" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;;; flx
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(setq flx-ido-threshhold 12000)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;; set gc threshold to 20mb
(setq gc-cons-threshold 20000000)

(provide 'init-ido)
