;;;; Package guide-key
(sm-package guide-key
            :package-manager "package"
            :unmanaged-p nil)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c p" "C-x 8"))
(setq guide-key/recursive-key-sequence-flag t)
(guide-key-mode 1)  ; Enable guide-key-mode

(sm-provide :package guide-key)
