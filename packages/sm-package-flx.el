;;;; Package flx
(sm-package flx
            :package-manager "package"
            :unmanaged-p nil)

(require 'flx-ido)
;;(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
;; (setq ido-use-faces t)

(sm-provide :package flx)
