;;;; This module should contain your basic configuration, that will be
;;;; shared by all the profiles - if they include the base module, of
;;;; course!

(sm-module "base"
           ;; add the packages required by your basic configuration here
           :require-packages '("evil" "magit" "smex" "ido-ubiquitous"
                               "gitignore-mode" "parenface" "s" "wgrep"
                               "pp-c-l")
           ;; set this to t if you want to manage this module yourself
           ;; instead of using the builtin package loading infrastructure
           :unmanaged-p nil)


;;;; Remove these 2 blocks if the module is unmanaged
(sm-module-pre (base)
  (ido-mode t)
  )

(sm-module-post (base)
  ;; smotitah aliases
  (defalias 'em 'sm-edit-module)
  (defalias 'ep 'sm-edit-package)
  (defalias 'epr 'sm-edit-profile)
  ;; ido
  (setq ido-enable-flex-matching t)
  ;; insert titled comment
  (defvar *titled-comment-length* 90)
  (defun insert-titled-comment (string)
    (interactive "sTitle: ")
    (let* ((clen (length string))
           (comment-prefix (format "%s " (s-repeat 4 comment-start)))
           (remaining-space (- *titled-comment-length* (length comment-prefix) (+ 2 clen)))
           (dashes-left (replicate "-" (floor (/ remaining-space 2.0))))
           (dashes-right (replicate "-" (ceiling (/ remaining-space 2.0)))))
      (insert (format "%s%s %s %s\n" comment-prefix dashes-left string dashes-right))))
  ;; column number mode
  (column-number-mode t)
  ;; global bindings
  (define-key key-translation-map (kbd "C-.") (kbd "M-TAB"))
  (global-set-key [f7] 'magit-status)
  (global-set-key (kbd "C-\;") 'message-point)
  )

(sm-provide :module base)
;;;; End base module

