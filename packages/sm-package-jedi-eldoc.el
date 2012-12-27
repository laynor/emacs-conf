;;;; Package jedi-eldoc
(sm-package jedi-eldoc
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/jedi-eldoc/"))

(require 'jedi-eldoc)

(sm-provide :package jedi-eldoc)
