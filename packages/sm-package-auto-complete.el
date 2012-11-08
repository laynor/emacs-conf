;;;; Package auto-complete
(sm-package auto-complete
            :package-manager "package"
            :unmanaged-p nil)

(require 'auto-complete-config)

(ac-config-default)

(set-face-underline 'ac-candidate-face "grey")

(sm-provide :package auto-complete)
