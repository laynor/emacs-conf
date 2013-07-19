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

(sm-integrate-with xgtags
  (define-key projectile-mode-map  (kbd "C-c p r") 'xgtags-query-replace-regexp))

(defvar projectile--compile-history-cache-location
  "projectile-compile-history/")
(defun projectile-compile-project ()
  "Run project compilation command."
  (interactive)
  (let* ((project-root (projectile-project-root))
	 (cache-location (directory-file-name
			  (replace-regexp-in-string
			   "/+" "/"
			   (concat projectile--compile-history-cache-location
				   (projectile-project-root)))))
	 (stored-compilation-cmds (persistent-soft-fetch 'compile-cmds
							 cache-location))
	 (compile-history stored-compilation-cmds)
         (compilation-cmd (compilation-read-command (projectile-compilation-command project-root))))
    (cd project-root)
    (puthash project-root compilation-cmd projectile-compilation-cmd-map)
    (persistent-soft-store 'compile-cmds (remove-duplicates
					  (cons compilation-cmd stored-compilation-cmds)
					  :test #'equal)
			   cache-location)
    (compilation-start compilation-cmd)))

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
