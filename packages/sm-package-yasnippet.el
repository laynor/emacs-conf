;;;; Package yasnippet
(sm-package yasnippet
            :package-manager "package"
            :unmanaged-p nil)

(require 'yasnippet)

(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'yas-minor-mode-hook 'yas-reload-all)

(yas-reload-all)

(sm-provide :package yasnippet)
