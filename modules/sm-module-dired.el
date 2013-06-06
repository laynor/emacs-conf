;;;; Module dired
(sm-module dired
           :unmanaged-p nil
           :require-packages '("dired" "dired-details+" "dired-details" "dired-efap" "dired-single"))

(sm-module-pre (dired)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (dired)
  ;; TODO insert your post-package-initialization code here
  )

(sm-provide :module dired)
