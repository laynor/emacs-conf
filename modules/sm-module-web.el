;;;; Module web
(sm-module web
           :unmanaged-p nil
           :require-packages '(web-mode))
           ;; :require-packages '(nxhtml zencoding))

(sm-module-pre (web)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (web)
  ;; TODO insert your post-package-initialization code here
  )

(sm-provide :module web)
