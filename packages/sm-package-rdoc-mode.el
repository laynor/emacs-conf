;;;; Package rdoc-mode
(sm-package rdoc-mode
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat (getenv "HOME") "/.rvm/src/" (getenv "rvm_ruby_string") "/misc/"))
(require 'rdoc-mode)

;;; TODO insert your package initialization code here

(sm-provide :package rdoc-mode)
