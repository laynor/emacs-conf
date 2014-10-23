;;;; Package emacs-gradle-mode
(sm-package emacs-gradle-mode
            :package-manager nil
            :unmanaged-p t)
(add-to-list 'load-path "/home/ale/.emacs.d/site-lisp/emacs-gradle-mode")
(require 'gradle-mode)
;;; TODO insert your package initialization code here

(sm-provide :package emacs-gradle-mode)
