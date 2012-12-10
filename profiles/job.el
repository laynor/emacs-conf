;;; smotitah: profile home-windows

;;; Before loading modules
(sm-profile-pre (job)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  )

;;; Modules to activate
(sm-require-modules "base" "elisp" "lightware" "C-job")

;;; After loading modules
(sm-profile-post (job)
  (load "custom.el"))



;;;; sm-base-profile end.
