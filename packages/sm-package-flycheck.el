;;;; Package flycheck
(sm-package flycheck
  :package-manager "package"
  :unmanaged-p nil)

;; Load flycheck
(require 'flycheck)

(put 'flycheck-error-overlay 'line-prefix nil)
(put 'flycheck-warning-overlay 'line-prefix nil)

(sm-provide :package flycheck)
