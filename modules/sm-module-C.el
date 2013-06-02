;;;; Module C
(sm-module C
           :unmanaged-p nil
           :require-packages '(yasnippet
                               ;; auto-complete-clang-async
                               auto-complete-clang
                               c-eldoc))

(sm-module-pre (C)
  )

(sm-module-post (C)

  (defvar c-get-standard-include-dirs-command
    "echo | cpp -x c++ -Wp,-v 2>&1 | grep '^.*include' | grep -v '^\\(ignoring\\|#include\\)' | sed 's/^ //g'"
    "Command used to retrieve the standard C/C++ include directories.")

  (defun c-get-standard-include-dirs ()
    "Retrieves the standard C/C++ include directories"
    (mapcar (lambda (include-path)
              (concat "-I" include-path))
            (split-string (shell-command-to-string c-get-standard-include-dirs-command) "\n" t)))

  (setq ac-clang-flags (c-get-standard-include-dirs))

  (setq c-eldoc-includes (concat c-eldoc-includes " "
                                 (mapconcat 'identity (c-get-standard-include-dirs) " ")))
  (add-hook 'c-mode-common-hook 'yas-minor-mode-on)

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

)

(sm-provide :module C)
