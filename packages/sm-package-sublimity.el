;;;; Package nurumacs
(sm-package sublimity
            :package-manager "package"
            :unmanaged-p nil)

(require 'sublimity nil t)
(sublimity-scroll)
(sublimity-global-mode)

(sm-provide :package sublimity)
