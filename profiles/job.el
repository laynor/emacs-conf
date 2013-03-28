;;; smotitah: profile home-windows

;;; Before loading modules
(sm-profile-pre (job)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (setq erlang-root-dir "~/local/lib/erlang")
  (setq exec-path (cons "~/local/lib/erlang/bin" exec-path))
  )

;;; Modules to activate
(sm-require-modules "base" "elisp" "dired" "lightware" "C" "objc" "erlang" "python" "common-lisp" "lua" "ruby" "diminish" "org")

;;; After loading modules
(sm-profile-post (job)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/cmake/")
  (require 'cmake-mode)
  (add-to-list 'auto-mode-alist (cons "\\.cmake" 'cmake-mode))
  (add-to-list 'auto-mode-alist (cons "CMakeLists.txt" 'cmake-mode))
  (load "custom.el"))



;;;; sm-base-profile end.
