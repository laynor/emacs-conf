;;;; Package wgrep-ack
(sm-package wgrep-ack
            :package-manager "package"
            :unmanaged-p nil)

(add-hook 'ag-mode-hook 'wgrep-ack-and-a-half-setup)

(sm-provide :package wgrep-ack)
