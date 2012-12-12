;;;; Package fuel
(sm-package fuel
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path "/usr/lib/factor/misc/fuel")
(load "fu")

(sm-provide :package fuel)
