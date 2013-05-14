;;;; Module C
(sm-module C
           :unmanaged-p nil
           :require-packages '(yasnippet auto-complete-clang-async c-eldoc))

(sm-module-pre (C)
  )

(sm-module-post (C)
  (defvar ac-clang-debug nil)

  (defun ac-clang-handle-error (res args)
    (goto-char (point-min))
    (let* ((buf (get-buffer-create ac-clang-error-buffer-name))
           (cmd (concat ac-clang-executable " " (mapconcat 'identity args " ")))
           (pattern (format ac-clang-completion-pattern ""))
           (err (if (re-search-forward pattern nil t)
                    (buffer-substring-no-properties (point-min)
                                                    (1- (match-beginning 0)))
                  ;; Warn the user more agressively if no match was found.
                  (when ac-clang-debug
                    (message "clang failed with error %d:\n%s" res cmd))
                  (buffer-string))))

      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (current-time-string)
                  (format "\nclang failed with error %d:\n" res)
                  cmd "\n\n")
          (insert err)
          (setq buffer-read-only t)
          (goto-char (point-min))))))

  (defvar c-get-standard-include-dirs-command
    "echo | cpp -x c++ -Wp,-v 2>&1 | grep '^.*include' | grep -v '^\\(ignoring\\|#include\\)' | sed 's/^ //g'"
    "Command used to retrieve the standard C/C++ include directories.")

  (defun c-get-standard-include-dirs ()
    "Retrieves the standard C/C++ include directories"
    (mapcar (lambda (include-path)
              (concat "-I" include-path))
            (split-string (shell-command-to-string c-get-standard-include-dirs-command) "\n" t)))

  (defun pj-include-dirs (pjbase)
    (let ((pjbase (or pjbase (getenv "PJBASE"))))
      (list
       (concat pjbase "pjmedia/include")
       (concat pjbase "pjlib/include")
       (concat pjbase "pjlib-util/include")
       (concat pjbase "pjsip/include")
       (concat pjbase "pjnath/include"))))

  (defun pj-add-include-dirs (pjbase)
    (interactive (list (read-directory-name "PJProject root path: " (or (getenv "PJBASE") (getenv "PWD")))))
    (let ((pj-include-dirs (pj-include-dirs (file-truename (file-name-as-directory pjbase)))))
      (setq c-eldoc-includes (concat c-eldoc-includes " "
                                     (mapconcat 'identity pj-include-dirs " "))

            ac-clang-flags (append (mapcar (lambda (ip) (concat "-I" ip))
                                           pj-include-dirs)
                                   ac-clang-flags))))

  (setq ac-clang-flags (c-get-standard-include-dirs))

  (setq c-eldoc-includes (concat c-eldoc-includes " "
                                 (mapconcat 'identity (c-get-standard-include-dirs) " ")))
  (add-hook 'c-mode-common-hook 'yas-minor-mode-on)
  (add-hook 'c-mode-common-hook 'turn-on-fixme-mode)
  ;;(add-hook 'c-mode-common-hook 'add-my-include-directories)
  )

  (defun add-my-include-directories ()
    (interactive)
    (setq c-eldoc-includes (concat c-eldoc-includes " "
                                   (mapconcat '(lambda (dir) (concat "-I" dir))
                                              my-include-directories
                                              " "))

          ac-clang-flags (append (mapcar (lambda (ip) (concat "-I" ip))
                                         my-include-directories)
                                 ac-clang-flags)))

(defun add-project-directories (&rest args)
  (defvar my-include-directories nil)
  (make-variable-buffer-local 'my-include-directories)
  (setq my-include-directories
        (mapcar (lambda (subdir)
                  (concat (file-name-directory
                           (file-truename (locate-dominating-file buffer-file-name ".dir-locals.el")))
                          subdir))
                args))
  (add-my-include-directories))

(sm-provide :module C)
