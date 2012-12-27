;;; smotitah: profile home-windows

;;; Before loading modules
(sm-profile-pre (home-linux)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (setq erlang-root-dir "/usr/lib/erlang")
  (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
  )

;;; Modules to activate
(sm-require-modules "base" "elisp" "C" "factor" "erlang" "python")

;;; After loading modules
(sm-profile-post (home-linux)
  (custom-set-variables
   '(jedi:server-command (quote ("python2" "/home/ale/.emacs.d/elpa/jedi-20121212.2016/jediepcserver.py")))
   '(python-shell-interpreter "python2"))

  (load "custom.el"))



;;;; sm-base-profile end.
