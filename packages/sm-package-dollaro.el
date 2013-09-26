;;;; Package dollaro
(sm-package dollaro
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (local-repo "dollaro"))
(load-library "dollaro")

(sm-provide :package dollaro)
