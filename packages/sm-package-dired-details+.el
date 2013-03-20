;;;; Package dired-details+
(sm-package dired-details+
            :package-manager "package"
            :unmanaged-p nil)

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
(define-key dired-mode-map (kbd "M-h") 'dired-hide-details-mode)

(sm-provide :package dired-details+)
