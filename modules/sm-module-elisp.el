;;;; Module elisp
(sm-module elisp
           :unmanaged-p nil
           :require-packages '("elisp-slime-nav"))

(sm-module-pre (elisp)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (elisp)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (defun message-point()
    (interactive)
    (message "Point: %S" (point)))
  
  )

(sm-provide :module elisp)
               
               
               
