;;;; Package outshine
(sm-package outshine
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/outshine/"))
(require 'outshine)

(sm-provide :package outshine)
