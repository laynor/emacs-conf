;;; smotitah: profile home-windows

;;; Before loading modules
(sm-profile-pre (home-windows)
  (setq flymake-mode nil)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (push  "C:/Users/ale/local/opt/Msys2/usr/bin" exec-path)
  (setenv "PATH" (concat "C:\\Users\\ale\\local\\opt\\Msys2\\usr\\bin;"
                         (getenv "PATH")))
  (require 'server)
  (server-start)
  )

;;; Modules to activate
(sm-require-modules "base" "elisp" "c-sharp" "javascript" "web" "clojure")

;;; After loading modules
(sm-profile-post (home-windows)
  (toggle-frame-maximized)
  (setq browse-url-firefox-program "C:\\Program Files (x86)\\Mozilla Firefox\\firefox.exe")
  ;; (setq magit-git-executable "c:/cygwin/bin/git.exe")
  (load custom-file)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "microsoft" :family "Consolas")))))
  )

;;;; sm-base-profile end
