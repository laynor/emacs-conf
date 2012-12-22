;;;; Module C
(sm-module C
           :unmanaged-p nil
           :require-packages '(auto-complete-clang))

(sm-module-pre (C)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (C)
  ;; TODO insert your post-package-initialization code here
  )

(sm-provide :module C)
