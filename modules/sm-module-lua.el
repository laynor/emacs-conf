;;;; Module lua
(sm-module lua
           :unmanaged-p nil
           :require-packages '(lua-mode))

(sm-module-pre (lua)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (lua)
  ;; TODO insert your post-package-initialization code here
  )

(sm-provide :module lua)
