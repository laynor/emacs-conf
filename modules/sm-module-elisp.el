;;;; Module elisp
(sm-module elisp
           :unmanaged-p nil
           :require-packages '("elisp-slime-nav" "ale-fixme" "auto-complete"))

(sm-module-pre (elisp)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (elisp)
  ;; Utility function to display point value on minibuffer
  (defun message-point()
    (interactive)
    (message "Point: %S" (point)))

  ;; Activate minor modes
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode) ;eldoc
  (add-hook 'emacs-lisp-mode-hook (lambda () (fic-ext-mode 1))) ;fic-ext-mode
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  )

(sm-provide :module elisp)
