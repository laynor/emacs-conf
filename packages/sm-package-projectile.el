;;;; Package projectile
(sm-package projectile
            :package-manager "package"
            :unmanaged-p nil)

(projectile-global-mode t)

(sm-integrate-with (:package direx)
  (defun projectile-direx ()
    (interactive)
    (popwin:direx (projectile-project-root)))
  (global-set-key (kbd "C-c p D") 'projectile-direx)
  (easy-menu-add-item nil '("Tools" "Projectile")
                      ["Open project in direx" projectile-direx]
                      "Find in project (grep)"))

;; (defvar projectile-project-cleaning-commands
;;   '(("./rebar clean" .
;;      (lambda (dir)
;;        (file-exists-p (expand-file-name "rebar" dir))))
;;     ("rebar clean" .
;;      (lambda (dir)
;;        (and (executable-find "rebar")
;;             (file-exists-p (expand-file-name "rebar.config" dir)))))
;;     ("make clean" .
;;      (lambda (dir)
;;        (or (file-exists-p (expand-file-name "GNUmakefile"))
;;            (file-exists-p (expand-file-name "Makefile" dir)))))
;;     )
;;   "A list of pairs of commands and prerequisite lambdas to perform project cleaning.")
;; (add-to-list 'projectile-project-compilation-commands
;;      (cons "make"
;;            (lambda (dir)
;;              (file-exists-p (expand-file-name "GNUmakefile" dir)))))

;; (add-to-list 'projectile-project-test-commands
;;              (cons "make test"
;;                    (lambda (dir)
;;                      (file-exists-p (expand-file-name "GNUmakefile" dir)))))


;; ;; Rake
;; (add-to-list 'projectile-project-compilation-commands
;;              (cons "rake build"
;;                    (lambda (dir)
;;                      (file-exists-p (expand-file-name "Rakefile" dir)))))

;; (add-to-list 'projectile-project-test-commands
;;              (cons "rake spec"
;;                    (lambda (dir)
;;                      (file-exists-p (expand-file-name "Rakefile" dir)))))

;; (defun projectile-compile-project (&optional arg)
;;   "Run project compilation command."
;;   (interactive "p")
;;   (case arg
;;     (4 (projectile-run-project-command projectile-project-cleaning-commands))
;;     (otherwise (projectile-run-project-command projectile-project-compilation-commands))))

(sm-provide :package projectile)
