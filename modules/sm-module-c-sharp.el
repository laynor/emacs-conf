;;;; Module c-sharp
(sm-module c-sharp
           :unmanaged-p nil
           :require-packages '("csharp-mode"))

(sm-module-pre (c-sharp)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (c-sharp)
  (add-to-list 'auto-mode-alist (cons "\\.cs$" 'csharp-mode))

  )

(sm-provide :module c-sharp)
