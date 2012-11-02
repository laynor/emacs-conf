;;;; Package lightware
(sm-package lightware
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/lightware/"))
(defun lw-reload ()
  (interactive)
  (mapc 'load '("lw-faces" "lw-overlays" "lw-data" "lw-commands"))
  (setq lw-snippets nil)
  (remove-overlays (point-min) (point-max)))

(sm-provide :package lightware)
