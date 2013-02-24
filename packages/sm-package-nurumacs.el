;;;; Package nurumacs
(sm-package nurumacs
            :package-manager "package"
            :unmanaged-p nil)

(require 'nurumacs)
(setq nurumacs-vdecc 2.7)
(setq nurumacs-map nil)

(sm-provide :package nurumacs)
