;;;; Package auto-complete-distel
(sm-package auto-complete-distel
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/auto-complete-distel/"))
(require 'auto-complete-distel)

(defun ac-distel-setup ()
  (setq ac-sources '(ac-source-distel)))

(sm-provide :package auto-complete-distel)
