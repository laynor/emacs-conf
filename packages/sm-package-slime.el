;;;; Package slime
(sm-package slime
            :unmanaged-p t)

(load "~/quicklisp/slime-helper.el")
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-lisp-implementations
      '((sbcl ("sbcl"))
        (clojure ("clojure") :init swank-clojure-init)))

(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-asdf))

(require 'slime)

(global-set-key [f6] 'slime)

;;; Show hyperspec in eww
(defadvice slime-hyperspec-lookup (around browse-with-eww activate)
  (flet ((browse-url (url) (eww-browse-url url)))
    ad-do-it))

(sm-provide :package slime)
