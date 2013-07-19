;;;; Module haskell
(sm-module haskell
           :unmanaged-p nil
           :require-packages '(haskell-mode ghc ghci-completion))

(sm-module-pre (haskell)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (haskell)
  ;; TODO insert your post-package-initialization code here
  )

(sm-provide :module haskell)
