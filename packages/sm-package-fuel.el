;;;; Package fuel
(sm-package fuel
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path "/usr/lib/factor/misc/fuel")
(require 'factor-mode)
(require 'fuel-mode)
(setq fuel-factor-root-dir "/usr/lib/factor")


(sm-provide :package fuel)
