;;;; Module factor
(sm-module factor
           :unmanaged-p nil
           :require-packages '(fuel))

(sm-module-pre (factor)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (factor)
  (fuel-autodoc-mode 1)
  )

(sm-provide :module factor)
