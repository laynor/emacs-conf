;;;; Package shell-pop
(sm-package shell-pop
            :package-manager "package"
            :unmanaged-p nil)

(require 'shell-pop)

(shell-pop-set-universal-key (kbd "<f2>"))
(shell-pop-set-internal-mode "eshell")
(shell-pop-set-window-position "bottom")

(sm-provide :package shell-pop)
