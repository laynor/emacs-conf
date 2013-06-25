;;;; Package rainbow-mode
(sm-package rainbow-mode
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/rainbow-mode/"))

(require 'rainbow-mode)

(add-hook 'emacs-lisp-mode-hook #'(lambda () (rainbow-mode t)))

(sm-provide :package rainbow-mode)
