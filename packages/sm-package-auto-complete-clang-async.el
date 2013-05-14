;;;; Package auto-complete-clang-async
(sm-package auto-complete-clang-async
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path
             (concat user-emacs-directory "site-lisp/emacs-clang-complete-async"))

(require 'auto-complete-clang-async)
;(setq ac-source-clang-async
      ;(append '((document . nil))
              ;ac-source-clang-async))

;(defadvice ac-clang-candidate (after ac-clang-candidate-show-signature-in-summary activate)
  ;(setq ad-return-value
        ;(mapcar (lambda (item) (propertize item
                                           ;'summary (ac-clang-document item)))
                ;ad-return-value)))

(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang-async ac-source-yasnippet) ac-sources))
  (ac-clang-launch-completion-process))

(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

(sm-provide :package auto-complete-clang-async)
