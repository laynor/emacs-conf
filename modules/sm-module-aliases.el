;;;; Module aliases
(sm-module aliases
           :unmanaged-p nil
           :require-packages '())

(sm-module-pre (aliases)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (aliases)
  ;; TODO insert your post-package-initialization code here
  (defalias 'smem 'sm-edit-module)
  (defalias 'smep 'sm-edit-package)
  (defalias 'smepr 'sm-edit-profile)
  (defalias 'im 'sm-integrate-module)
  )

(sm-provide :module aliases)
               
               
               
