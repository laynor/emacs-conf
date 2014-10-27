;;;; Package list-packages-ext
(sm-package list-packages-ext
            :package-manager nil
            :unmanaged-p t)

(load "~/.emacs.d/site-lisp/list-packages-ext/list-packages-ext.el")
(add-hook 'package-menu-mode-hook '(lambda () (list-packages-ext-mode 1)))

(sm-provide :package list-packages-ext)
