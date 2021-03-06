;;;; Package enotify
(sm-package enotify
            :package-manager nil
            :unmanaged-p t)

(setq enotify-mode-line-suffix "")
(setq enotify-mode-line-prefix "")

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/enotify/"))
(setq enotify-use-next-available-port t)
(require 'enotify)
(require 'enotify-tdd)
(enotify-minor-mode t)

(sm-provide :package enotify)
