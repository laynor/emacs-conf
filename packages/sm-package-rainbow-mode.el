;;;; Package rainbow-mode
(sm-package rainbow-mode
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/rainbow-mode/"))

(require 'rainbow-mode)

(sm-provide :package rainbow-mode)
