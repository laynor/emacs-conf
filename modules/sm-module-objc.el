;;;; Module objc
(sm-module objc
           :unmanaged-p nil
           :require-packages '(auto-complete auto-complete-clang yasnippet c-eldoc))

(sm-module-pre (objc)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (objc)
  (font-lock-add-keywords 'objc-mode '(("\\(@synthesize\\|@property\\|@autoreleasepool\\)" 1 'font-lock-keyword-face)))
  (add-hook 'objc-mode-hook '(lambda ()
                               (setq c-basic-offset 4)
                               (auto-complete-mode 1)))
  )

(sm-provide :module objc)
