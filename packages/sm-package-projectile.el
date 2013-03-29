;;;; Package projectile
(sm-package projectile
            :package-manager "package"
            :unmanaged-p nil)

(projectile-global-mode t)


(defvar projectile-project-cleaning-commands
  '(("./rebar clean" .
     (lambda (dir)
       (file-exists-p (expand-file-name "rebar" dir))))
    ("rebar clean" .
     (lambda (dir)
       (and (executable-find "rebar")
            (file-exists-p (expand-file-name "rebar.config" dir)))))
    ("make clean" .
     (lambda (dir)
       (or (file-exists-p (expand-file-name "GNUmakefile"))
           (file-exists-p (expand-file-name "Makefile" dir)))))
    )
  "A list of pairs of commands and prerequisite lambdas to perform project cleaning.")
(add-to-list 'projectile-project-compilation-commands
     (cons "make"
           (lambda (dir)
             (file-exists-p (expand-file-name "GNUmakefile" dir)))))

(add-to-list 'projectile-project-test-commands
             (cons "make test"
                   (lambda (dir)
                     (file-exists-p (expand-file-name "GNUmakefile" dir)))))

(defun projectile-compile-project (&optional arg)
  "Run project compilation command."
  (interactive "p")
  (case arg
    (4 (projectile-run-project-command projectile-project-cleaning-commands))
    (otherwise (projectile-run-project-command projectile-project-compilation-commands))))

(sm-provide :package projectile)
