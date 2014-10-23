;;;; Module mail
(sm-module mail
           :unmanaged-p nil
           :require-packages '(wanderlust)) ;;bbdb gnus))

(sm-module-pre (mail)
  ;; TODO insert your pre-package-initialization code here
  (setq wm-summary-width 150)
  )

(sm-module-post (mail)
  ;; TODO insert your post-package-initialization code here
  )

(sm-provide :module mail)
