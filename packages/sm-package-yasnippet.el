;;;; Package yasnippet
(sm-package yasnippet
            :package-manager "package"
            :unmanaged-p nil)

(require 'yasnippet)

(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(yas-reload-all)

(sm-provide :package yasnippet)
