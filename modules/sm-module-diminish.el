;;;; Module diminish
(sm-module diminish
           :unmanaged-p nil
           :require-packages '(diminish))

(sm-module-pre (diminish)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (diminish)
  (diminish 'projectile-mode)
  (diminish 'undo-tree-mode)
  (diminish 'guide-key-mode)
  (diminish 'elisp-slime-nav-mode)
  )

(sm-provide :module diminish)
