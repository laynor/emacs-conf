;; (setq *active-profile* (or (getenv "EMACS_PROFILE")
;; 			   "default-profile"))
;; (defadvice package-activate (before antani (package min-version) activate)
;;   (when (eq package 'mediawiki)
;;     (debug))
;;   (message "PACKAGE %s" package))
(add-to-list 'load-path user-emacs-directory)
;; (setq custom-file (concat user-emacs-directory *active-profile* ".el"))
;;(load *active-profile*)

;;(setq package-enable-at-startup nil)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'load-path (concat user-emacs-directory "smotitah"))
(setq sm-debug t)
(require 'smotitah)
;; (add-hook 'kill-emacs-hook 'sm-recompile-all)
(sm-initialize)

(require 'server)
(unless (server-running-p)
  (server-start))
