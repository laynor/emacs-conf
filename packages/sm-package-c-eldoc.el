;;;; Package c-eldoc
(sm-package c-eldoc
            :package-manager "package"
            :unmanaged-p nil)

(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(sm-provide :package c-eldoc)
