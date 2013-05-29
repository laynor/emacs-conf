;;;; Package linkd
(sm-package linkd
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/linkd"))
(require 'linkd)

(add-hook 'prog-mode-hook
          #'(lambda () (linkd-mode 1)))


(sm-provide :package linkd)
