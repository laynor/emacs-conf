;;;; Package enotify
(sm-package enotify
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/enotify/"))
(setq enotify-use-next-available-port t)
(require 'enotify)
(enotify-minor-mode t)

(sm-provide :package enotify)
