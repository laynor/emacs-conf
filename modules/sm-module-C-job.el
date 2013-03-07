;;;; Module C
(sm-module C-job
           :unmanaged-p nil
           :require-packages '(ToolBOS yasnippet auto-complete auto-complete-clang c-eldoc))

(sm-module-pre (C-job)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (C-job)
  (defvar c-get-standard-include-dirs-command
    "echo | cpp -x c++ -Wp,-v 2>&1 | grep '^.*include' | grep -v '^\\(ignoring\\|#include\\)' | sed 's/^ //g'"
    "Command used to retrieve the standard C/C++ include directories.")

  (defun pj-include-dirs (pjbase)
    (let ((pjbase (or pjbase (getenv "PJBASE"))))
      (list
       (concat pjbase "pjmedia/include")
       (concat pjbase "pjlib/include")
       (concat pjbase "pjlib-util/include")
       (concat pjbase "pjsip/include")
       (concat pjbase "pjnath/include"))))

;; ./third_party/resample/include
;; ./third_party/speex/include
;; ./third_party/srtp/crypto/include
;; ./third_party/srtp/include
;; ./third_party/portaudio/bindings/cpp/include
;; ./third_party/portaudio/include

  (defun pj-add-include-dirs (pjbase)
    (interactive "D")
    (let ((pj-include-dirs (pj-include-dirs (file-truename (file-name-as-directory pjbase)))))
      (setq c-eldoc-includes (concat c-eldoc-includes " "
                                     (mapconcat 'identity pj-include-dirs " "))

            ac-clang-flags (append (mapcar (lambda (ip) (concat "-I" ip))
                                           pj-include-dirs)
                                   ac-clang-flags))))

  (defun c-get-standard-include-dirs ()
    "Retrieves the standard C/C++ include directories"
    (mapcar (lambda (include-path)
              (concat "-I" include-path))
            (split-string (shell-command-to-string c-get-standard-include-dirs-command) "\n" t)))




  (setq ac-clang-flags (c-get-standard-include-dirs))

  (setq c-eldoc-includes (concat c-eldoc-includes " "
                                 (mapconcat 'identity (c-get-standard-include-dirs) " ")))

  (add-hook 'c-mode-common-hook 'yas-minor-mode-on)
  (add-hook 'c-mode-common-hook 'turn-on-fixme-mode)

  )

(sm-provide :module C-job)
