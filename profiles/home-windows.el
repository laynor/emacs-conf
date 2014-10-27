;;; smotitah: profile home-windows

;;; Before loading modules
(sm-profile-pre (home-windows)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (setenv "PATH" (concat "C:\\Users\\ale\\local\\opt\\Msys2\\usr\\bin;"
                         (getenv "PATH")))
  (push "C:\\Users\\ale\\local\\opt\\Msys2\\usr\\bin;" exec-path)
  (require 'server)
  (server-start)

  ;; (setenv "PATH" (concat   "c:\\Users\\ale\\AppData\\Local\\scoop\\apps\\findutils\\4.4.2\\bin;"
  ;;                          "c:\\Users\\ale\\AppData\\Local\\scoop\\apps\\diffutils\\2.8.7\\bin;"
  ;;                          "c:\\Users\\ale\\AppData\\Local\\scoop\\apps\\grep\\2.5.4\\bin;"
  ;;                          (getenv "PATH")))
  )

;;; Modules to activate
(sm-require-modules "base" "elisp" "c-sharp" "org")

;;; After loading modules
(sm-profile-post (home-windows)
  (setq browse-url-firefox-program "C:\\Program Files (x86)\\Mozilla Firefox\\firefox.exe")
  ;; (setq magit-git-executable "c:/cygwin/bin/git.exe")
  (load (concat user-emacs-directory "custom.el")))

;;;; sm-base-profile end
