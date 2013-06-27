;;;; Package helm
(sm-package helm
            :package-manager "package"
            :unmanaged-p nil)

(require 'helm-config)
(helm-mode 1)

(sm-provide :package helm)
