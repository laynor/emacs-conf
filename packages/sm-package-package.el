;;;; Package package
(sm-package package
            :package-manager "builtin"
            :unmanaged-p nil)

(defvar package-manually-installed-packages-file
  (concat user-emacs-directory ".manually-installed-packages"))

(defun package-list ()
  (sort (remove-duplicates (mapcar 'car package-alist) :test 'equal)
        (lambda (s1 s2)
          (string< (symbol-name s1)
                   (symbol-name s2)))))


(setq package-manually-installed-packages
  (if (file-exists-p package-manually-installed-packages-file)
    (with-temp-buffer
      (insert-file-contents package-manually-installed-packages-file)
      (car (read-from-string (buffer-string))))
    (package-list)))


(defun package-dump-manually-installed-packages ()
  (setq package-manually-installed-packages
        (sort (remove-duplicates package-manually-installed-packages)
              (lambda (s1 s2)
                (string< (symbol-name s1)
                         (symbol-name s2)))))
  (with-temp-file package-manually-installed-packages-file
    (insert "(\n")
    (dolist (el package-manually-installed-packages)
      (insert (format "%S\n" el)))
    (insert ")\n")))


(defadvice package-install (before track-manually-installed-packages (name) activate)
  (push name package-manually-installed-packages)
  (package-dump-manually-installed-packages))

(defun package-depends (package)
  "Retrieves the packages PACKAGE depends on."
  (mapcar 'car (elt (cdr (assoc package package-alist)) 1)))

(defun package-revdep (package)
  "Returns a list of the currently installed packages that
depend on PACKAGE."
  (let* ((package-list (mapcar 'car package-alist))
         (deps (mapcar (lambda (p) (cons p (package-depends p)))
                       package-list)))
    (mapcar 'car (remove-if-not (lambda (dep) (memq package (cdr dep))) deps))))


(defun package-manually-installed-p (package)
  (memq package package-manually-installed-packages))

(defun package-orphan-p (package)
  (let ((revdeps (package-revdep package)))
    (and (not (package-manually-installed-p package))
         (every 'package-orphan-p revdeps))))

(defun package-orphans ()
  "Returns a list of orphaned packages.
Does not work right now, as it returns manually installed packages too."
  (remove-if-not 'package-orphan-p
                 (package-list)))

(defadvice package-initialize (after remove-uninstalled-pkgs-from-manually-installed-list
                                     (&optional no-activate) activate)
  (setq package-manually-installed-packages
        (intersection package-manually-installed-packages
                      (package-list)))
  (package-dump-manually-installed-packages))


(sm-provide :package package)
