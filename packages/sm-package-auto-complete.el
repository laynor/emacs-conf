;;;; Package auto-complete
(sm-package auto-complete
            :package-manager "package"
            :unmanaged-p nil)

(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories (concat myoptdir "AC/ac-dict"))


;; (ac-set-trigger-key "TAB")
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  ;; (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  ;; (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  ;; (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
;; ac-source-gtags
(my-ac-config)

(sm-provide :package auto-complete)
