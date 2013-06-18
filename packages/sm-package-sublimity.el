;;;; Package nurumacs
(sm-package sublimity
            :package-manager "package"
            :unmanaged-p nil)

(require 'sublimity nil t)
(sublimity-scroll)

(sm-provide :package sublimity)
