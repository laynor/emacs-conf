;;;; Module lua
(sm-module lua
           :unmanaged-p nil
           :require-packages '(lua-mode))

(sm-module-pre (lua)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (lua)
  (add-hook 'lua-mode-hook 'rainbow-mode)
  )

(sm-provide :module lua)
