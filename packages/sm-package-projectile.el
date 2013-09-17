;;;; Package projectile
(sm-package projectile
            :package-manager "package"
            :unmanaged-p nil)

(projectile-global-mode t)

(defmacro* define-projectile-command (command-name lambda-list menu-item keybinding &body body)
  `(progn (defun ,command-name ,lambda-list
            ,@body)
          (define-key projectile-mode-map ,keybinding ',command-name)
          (easy-menu-add-item nil
                              '("Tools" "Projectile")
                              (vector ,menu-item ',command-name)
                              "Find in project (grep)")))

(sm-integrate-with (:package direx)
  (define-projectile-command projectile-direx () "Open project in Direx" (kbd "C-c p M")
    (interactive)
    (popwin:direx (projectile-project-root))))


(sm-integrate-with xgtags
  (define-key projectile-mode-map  (kbd "C-c p r") 'xgtags-query-replace-regexp))

(sm-integrate-with magit
  (define-projectile-command projectile-magit-to-project (arg)
    "Open project in magit" (kbd "C-c p m")
    (interactive "P")
    (let* ((project-to-switch
            (projectile-completing-read "Magit to project: "
                                        projectile-known-projects)))
      (magit-status project-to-switch))))

(defvar projectile--compile-history-cache-location
  "projectile-compile-history/")

(defun projectile-compile-project ()
  "Run project compilation command."
  (interactive)
  (let* ((project-root (projectile-project-root))
	 (buffers (projectile-project-buffers))
	 (cache-location (directory-file-name
			  (replace-regexp-in-string
			   "/+" "/"
			   (concat projectile--compile-history-cache-location
				   (projectile-project-root)))))
	 (stored-compilation-cmds (persistent-soft-fetch 'compile-cmds
							 cache-location))
	 (compile-history stored-compilation-cmds)
         (compilation-cmd (compilation-read-command
                           (projectile-compilation-command project-root))))
    (cd project-root)
    (save-some-buffers nil (lambda ()
			     (and (buffer-file-name)
				  (memq (current-buffer) buffers))))
    (puthash project-root compilation-cmd projectile-compilation-cmd-map)
    (persistent-soft-store 'compile-cmds (remove-duplicates
					  (cons (s-trim compilation-cmd) stored-compilation-cmds)
					  :test #'equal
					  :from-end t)
			   cache-location)
    (compilation-start compilation-cmd)))


(defun projectile--file-counterpart (basename)
  (let* ((basename-sans-extension (file-name-sans-extension basename))
	 (extension (file-name-extension basename))
	 (counterpart-extension (cond ((equal extension "c") "h")
				      ((equal extension "h") "c"))))
    (concat basename-sans-extension "." counterpart-extension)))

(defun projectile--find-file-with-find (filename)
  (split-string (shell-command-to-string  (format "find %S -name %S"
						  (projectile-project-root)
						  filename))
		"\n" t))



(defun projectile-switch-to-counterpart ()
  (interactive)
  (let* ((project-files (projectile-current-project-files))
	 (basename (file-name-nondirectory (buffer-file-name)))
	 (counterpart-basename (projectile--file-counterpart basename))
	 (counterpart (car (remove-if-not (lambda (file)
					    (equal (file-name-nondirectory file)
						   counterpart-basename))
					  project-files))))
    (when counterpart
      (find-file counterpart))))
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
