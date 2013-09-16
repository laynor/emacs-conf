(require 'cl)
(setq sm-profile "home-linux")

;; (setq *active-profile* (or (getenv "EMACS_PROFILE")
;; 			   "default-profile"))
;; (defadvice package-activate (before antani (package min-version) activate)
;;   (when (eq package 'mediawiki)
;;     (debug))
;;   (message "PACKAGE %s" package))
(defun local-repo (package-name)
  (concat user-emacs-directory "site-lisp/" package-name "/"))

(setq-default evil-intercept-esc 'always)
(add-to-list 'load-path user-emacs-directory)
;; (setq custom-file (concat user-emacs-directory *active-profile* ".el"))
;;(load *active-profile*)

;;(setq package-enable-at-startup nil)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'load-path (concat user-emacs-directory "smotitah"))
(setq sm-debug nil)
(require 'smotitah)
;; (add-hook 'kill-emacs-hook 'sm-recompile-all)
(sm-initialize)

;(require 'server)
;;; (eval-after-load "init"
;(unless (server-running-p)
  ;(message "mufu"))
  ;; (server-start))
;; (add-to-list 'load-path (local-repo "dollaro/"))
;; (load "dollaro")
(add-hook 'after-init-hook (lambda ()
			     (setq initial-scratch-message
                                   ($:fill-template (f-read-text
                                                     (concat user-emacs-directory
                                                             "scratch-template.$"))
                                                    `((init-time . ,(emacs-init-time))
                                                      (emacs-profile . ,sm-profile))))
                             (message "Startup time: %s" (emacs-init-time))))

(setq gc-cons-threshold 20000000)

(defalias 'smufu 'save-buffers-kill-emacs)
