;;;; Package outorg
(sm-package outorg
            :package-manager nil
            :unmanaged-p t)
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/outorg/"))
(require 'outorg)
;;; TODO insert your package initialization code here

(sm-provide :package outorg)
