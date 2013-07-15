;;;; Package melpa-upstream-visit
(sm-package melpa-upstream-visit
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (local-repo "melpa-upstream-visit"))
(require 'melpa-upstream-visit)

(sm-provide :package melpa-upstream-visit)
