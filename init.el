(require 'cl)
(defvar *emacs-load-start* (current-time))

;; (setq *active-profile* (or (getenv "EMACS_PROFILE")
;; 			   "default-profile"))
;; (defadvice package-activate (before antani (package min-version) activate)
;;   (when (eq package 'mediawiki)
;;     (debug))
;;   (message "PACKAGE %s" package))
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
(message "Startup time: %ds."
         (destructuring-bind (hi lo ms ps)
             (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

(setq gc-cons-threshold 20000000)

(defalias 'smufu 'save-buffers-kill-emacs)
