;;;; Package erlang
(sm-package erlang
            :package-manager "package"
            :unmanaged-p nil)

(evil-define-key 'insert erlang-mode-map (kbd ">")
  (lambda ()
    (interactive)
    (flet ((newline () nil))
      (call-interactively 'erlang-electric-gt))))


(add-hook 'erlang-mode-hook
          (lambda ()
            (setq inferior-erlang-machine-options '("-sname" "emacs"))))

(require 'erlang-start)


(sm-provide :package erlang)
