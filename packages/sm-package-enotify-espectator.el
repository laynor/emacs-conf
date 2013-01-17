;;;; Package enotify-espectator
(sm-package enotify-espectator
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
(require 'enotify-espectator)

(sm-provide :package enotify-espectator)
