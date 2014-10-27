;;;; Module scheme
(sm-module scheme
           :unmanaged-p nil
           :require-packages '())

(sm-module-pre (scheme)
  (add-hook 'scheme-mode-hook (lambda ()
                                ((geiser-mode -1)
                                 (slime-mode 1))))

  )

(sm-module-post (scheme)
  ;; TODO insert your post-package-initialization code here
  )

(sm-provide :module scheme)
