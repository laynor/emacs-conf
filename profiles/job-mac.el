;;; smotitah: profile home-windows
(sm-profile-pre (job-mac)
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (setq erlang-root-dir "~/local/lib/erlang")
  (setq exec-path (cons "~/local/lib/erlang/bin" exec-path))
  )

;;; Modules to activate
(sm-require-modules "base" "elisp" "dired"  "C" "erlang" "python" "org" "mac")

;;; After loading modules
(sm-profile-post (job-mac)
  (load custom-file)
  (set-face-font 'default "Menlo-10")
   ;; Fix dired
  (setq ls-lisp-use-insert-directory-program nil)
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  (require 'ls-lisp)
  (custom-set-variables
    '(powerline-height 16))
  (sm-integrate-with (:module python)
    (setq jedi:server-command (list "/usr/local/bin/python"
                                    jedi:server-script)))
  (add-to-list 'auto-mode-alist '("^Podfile$" . 'ruby-mode))
  )

;;;; sm-base-profile end.
