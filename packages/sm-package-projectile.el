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

(defun projectile--project-pcache-location (project)
  (concat
   (replace-regexp-in-string
    "/$" ""
    (replace-regexp-in-string
     "/+" "/"
     (format "%s%s"
             projectile--compile-history-cache-location
             project)))
   "/cache"))


(defun projectile--compilation-history (&optional project)
  (persistent-soft-fetch 'compilation-history
                         (projectile--project-pcache-location project)))

(defun projectile--remember-compilation-command (project command)
  (let* ((old-history (projectile--compilation-history project))
         (cleaned-history (remove-duplicates
                           (cons (s-trim command) old-history)
                           :test #'equal
                           :from-end t)))
    (puthash project-root compilation-cmd projectile-compilation-cmd-map)
    (persistent-soft-store 'compilation-history cleaned-history
                           (projectile--project-pcache-location project))))

(defun projectile-compile-project ()
  "Run project compilation command."
  (interactive)
  (let* ((project-root (projectile-project-root))
	 (project-buffers (projectile-project-buffers))
	 (stored-compilation-cmds (projectile--compilation-history project-root))
	 (compile-history stored-compilation-cmds)
         (compilation-cmd (compilation-read-command
                           (projectile-compilation-command project-root))))
    (cd project-root)
    (save-some-buffers nil (lambda ()
			     (and (buffer-file-name)
				  (memq (current-buffer) project-buffers))))
    (projectile--remember-compilation-command project-root
                                              compilation-cmd)
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


(sm-provide :package projectile)
