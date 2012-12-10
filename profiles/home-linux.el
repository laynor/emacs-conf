;;; smotitah: profile home-windows

;;; Before loading modules
(sm-profile-pre (home-linux)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  )

;;; Modules to activate
(sm-require-modules "base" "elisp" "C")

;;; After loading modules
(sm-profile-post (home-linux)
  (load "custom.el"))



;;;; sm-base-profile end.
