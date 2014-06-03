;;;; Module android
(sm-module android
           :unmanaged-p nil
           :require-packages '("groovy-mode"  "emacs-gradle-mode"))

(sm-module-pre (android)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (android)
  ;; TODO insert your post-package-initialization code here
  )

(sm-provide :module android)
