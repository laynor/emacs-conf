;;;; Package erlang
(sm-package erlang
            :package-manager "package"
            :unmanaged-p nil)

(add-hook 'erlang-mode-hook
          (lambda ()
            (setq inferior-erlang-machine-options '("-sname" "emacs"))))
(require 'erlang-start)


(sm-provide :package erlang)
