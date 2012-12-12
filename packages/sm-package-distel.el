;;;; Package distel
(sm-package distel
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/distel/elisp"))
(require 'distel)
(distel-setup)

(sm-provide :package distel)
