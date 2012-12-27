;;;; Package flycheck
(sm-package flycheck
  :package-manager "package"
  :unmanaged-p nil)

;; Load flycheck
(require 'flycheck)

(sm-provide :package flycheck)
