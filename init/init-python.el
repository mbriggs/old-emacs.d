;unload built in python

(when (featurep 'python) (unload-feature 'python t))

; load python-mode
(add-to-list 'load-path "~/.emacs.d/el-get/python-mode")
(setq py-install-directory  "~/.emacs.d/el-get/python-mode")
(setq py-load-pymacs-p t)
(require 'python-mode)

; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys nil)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)

; pyflakes

(setq flymake-python-pyflakes-executable "flake8")
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)


(provide 'init-python)
