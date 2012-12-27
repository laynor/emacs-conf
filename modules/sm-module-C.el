;;;; Module C
(sm-module C
           :unmanaged-p nil
           :require-packages '(auto-complete-clang c-eldoc))

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
  )

(sm-provide :module C)
