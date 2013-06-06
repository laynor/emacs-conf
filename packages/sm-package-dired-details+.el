;;;; Package dired-details+
(sm-package dired-details+
            :package-manager "package"
            :unmanaged-p nil)

(require 'dired-details+)
;; (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

(sm-provide :package dired-details+)
