;;;; Package surround
(sm-package surround
            :package-manager "package"
            :unmanaged-p nil)

(require 'surround)
(global-surround-mode t)

(sm-provide :package surround)
