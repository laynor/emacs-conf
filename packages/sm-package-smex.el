;;;; Package smex
(sm-package smex
            :package-manager "package"
            :unmanaged-p nil)

;;; TODO insert your package initialization code here
(progn
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex))

(sm-provide :package smex)
