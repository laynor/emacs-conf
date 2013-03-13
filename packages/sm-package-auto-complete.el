;;;; Package auto-complete
(sm-package auto-complete
            :package-manager "package"
            :unmanaged-p nil)

(require 'auto-complete-config)

(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(my-ac-config)
(define-key ac-mode-map (kbd "M-/") 'auto-complete)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(sm-provide :package auto-complete)
