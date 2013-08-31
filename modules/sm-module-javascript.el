;;;; Module javascript
(sm-module javascript
           :unmanaged-p nil
           :require-packages '("js3-mode" "ac-js2"))

(sm-module-pre (javascript)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (javascript)
  ;; TODO insert your post-package-initialization code here
  )

(sm-provide :module javascript)
