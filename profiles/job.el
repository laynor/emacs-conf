;;; smotitah: profile home-windows

;;; Before loading modules
(sm-profile-pre (job)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (setq erlang-root-dir "~/local/lib/erlang")
  (setq exec-path (cons "~/local/lib/erlang/bin" exec-path))
  )

;;; Modules to activate
(sm-require-modules "base" "elisp" "lightware" "C-job" "erlang" "python" "common-lisp" "lua")

;;; After loading modules
(sm-profile-post (job)
  (load "custom.el"))



;;;; sm-base-profile end.
