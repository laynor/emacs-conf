;;;; Package list-packages-ext
(sm-package list-packages-ext
            :package-manager nil
            :unmanaged-p t)

(require 'list-package-extras)
(add-hook 'package-menu-mode-hook '(lambda () (list-packages-ext-mode 1)))

(sm-provide :package list-packages-ext)
