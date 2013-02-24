;;;; Package guide-key
(sm-package guide-key
            :package-manager "package"
            :unmanaged-p nil)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1)  ; Enable guide-key-mode

(sm-provide :package guide-key)
