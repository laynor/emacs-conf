;;;; Package auto-complete-clang
(sm-package auto-complete-clang
            :package-manager "package"
            :unmanaged-p nil)

(require 'auto-complete-clang)

;; Do not insert function signature in the buffer
(setq ac-source-clang
      (append '((document . nil))
              ac-source-clang))

(defadvice ac-clang-candidate (after ac-clang-candidate-show-signature-in-summary activate)
  (setq ad-return-value
        (mapcar (lambda (item) (propertize item
                                           'summary (ac-clang-document item)))
                ad-return-value)))

(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)



(sm-provide :package auto-complete-clang)
