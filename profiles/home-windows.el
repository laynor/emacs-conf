;;; smotitah: profile home-windows

;;; Before loading modules
(sm-profile-pre (home-windows)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  )

;;; Modules to activate
(sm-require-modules "base")

;;; After loading modules
(sm-profile-post (home-windows)
  (setq magit-git-executable "c:/cygwin/bin/git.exe")
  (load "custom.el")
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "outline" :family "Consolas"))))))


;;;; sm-base-profile end.
