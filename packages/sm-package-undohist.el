;;;; Package undohist
(sm-package undohist
            :package-manager "package"
            :unmanaged-p nil)

(require 'undohist)
(undohist-initialize)

(sm-provide :package undohist)
