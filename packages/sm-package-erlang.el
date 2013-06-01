;;;; Package erlang
(sm-package erlang
            :package-manager "package"
            :unmanaged-p nil)

(require 'erlang)
(require 'erlang-start)

(define-key erlang-mode-map (kbd ">")
  (lambda ()
    (interactive)
    (flet ((newline () nil))
      (call-interactively 'erlang-electric-gt))))


(add-hook 'erlang-mode-hook
          (lambda ()
            (setq inferior-erlang-machine-options '("-sname" "emacs"))))

(sm-provide :package erlang)
