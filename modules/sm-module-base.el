;;;; This module should contain your basic configuration, that will be
;;;; shared by all the profiles - if they include the base module, of
;;;; course!

(sm-module "base"
           ;; add the packages required by your basic configuration here
           :require-packages '("evil" "magit" "smex" "ido-ubiquitous"
                               "gitignore-mode" "parenface" "s" "wgrep")
           ;; set this to t if you want to manage this module yourself
           ;; instead of using the builtin package loading infrastructure
           :unmanaged-p nil)


;;;; Remove these 2 blocks if the module is unmanaged
(sm-module-pre (base)
  (ido-mode t)
  )

(sm-module-post (base)
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (define-key key-translation-map (kbd "C-.") (kbd "M-TAB"))
  )

(sm-provide :module base)
;;;; End base module

