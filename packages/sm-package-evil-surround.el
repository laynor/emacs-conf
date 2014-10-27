;;;; Package surround
(sm-package evil-surround
            :package-manager "package"
            :unmanaged-p nil)

(require 'evil-surround)

(global-evil-surround-mode t)

(sm-provide :package evil-surround)
