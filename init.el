;; (setq *active-profile* (or (getenv "EMACS_PROFILE")
;; 			   "default-profile"))

(add-to-list 'load-path user-emacs-directory)
;; (setq custom-file (concat user-emacs-directory *active-profile* ".el"))
;;(load *active-profile*)

;;(setq package-enable-at-startup nil)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'load-path (concat user-emacs-directory "smotitah"))
(setq sm-debug t)
(require 'smotitah)
(sm-initialize)
