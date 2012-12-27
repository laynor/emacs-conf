;;;; Package popup-git
(sm-package popup-git
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/popup/"))

(require 'popup)

(setq popup-truncated-summary-postfix ">>>>")


(sm-provide :package popup-git)
