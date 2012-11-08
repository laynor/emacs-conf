;;;; This module should contain your basic configuration, that will be
;;;; shared by all the profiles - if they include the base module, of
;;;; course!

(sm-module "base"
           ;; add the packages required by your basic configuration here
           :require-packages '("melpa" "evil" "magit" "smex" "ido-ubiquitous"
                               "gitignore-mode" "parenface" "s" "wgrep"
                               "pp-c-l" "erc" "dired")
           ;; set this to t if you want to manage this module yourself
           ;; instead of using the builtin package loading infrastructure
           :unmanaged-p nil)


;;;; Remove these 2 blocks if the module is unmanaged
(sm-module-pre (base)
  (ido-mode t)
  )

(sm-module-post (base)
  (require 'uniquify)
  ;; smotitah aliases
  (defalias 'em 'sm-edit-module)
  (defalias 'ep 'sm-edit-package)
  (defalias 'epr 'sm-edit-profile)
  ;; insert titled comment
  (defvar *titled-comment-length* 90
    "Total width of comments inserted with `insert-titled-comment'")
  (defun insert-titled-comment (string)
    "Inserts a comment in the stile
;;;; --------------------------------- STRING expansion ----------------------------------
The number of dashes is calculated based on `*titled-comment-length*'.
"
    (interactive "sTitle: ")
    (let* ((clen (length string))
           (comment-prefix (format "%s " (s-repeat 4 comment-start)))
           (remaining-space (- *titled-comment-length* (length comment-prefix) (+ 2 clen)))
           (dashes-left (s-repeat (floor (/ remaining-space 2.0)) "-"))
           (dashes-right (s-repeat (ceiling (/ remaining-space 2.0)) "-")))
      (insert (format "%s%s %s %s\n" comment-prefix dashes-left string dashes-right))))
  ;; global bindings
  (define-key key-translation-map (kbd "C-.") (kbd "M-TAB"))
  (global-set-key [f7] 'magit-status)
  (global-set-key (kbd "C-\;") 'message-point)
  (defadvice magit-key-mode (after evil-magit-key-mode-in-emacs-state (for-group &optional original-opts) activate)
    (evil-emacs-state))
  )

(sm-provide :module base)
;;;; End base module

