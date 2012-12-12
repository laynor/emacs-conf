;;;; Package erlang
(sm-package erlang
            :package-manager "package"
            :unmanaged-p nil)

(add-hook 'erlang-mode-hook
          (lambda ()
            (setq inferior-erlang-machine-options '("-sname" "emacs"))))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)


(sm-provide :package erlang)
