;;;; Module web
(sm-module web
           :unmanaged-p nil
           :require-packages '(web-mode zencoding))

(sm-module-pre (web)
(push '("\\.html?$" . web-mode) auto-mode-alist)
  ;; TODO insert your pre-package-initialization code here
  (add-to-list 'auto-mode-alist (cons "\\.html?$" 'web-mode))
  )

(sm-module-post (web)
  ;; TODO insert your post-package-initialization code here
  )

(sm-provide :module web)
