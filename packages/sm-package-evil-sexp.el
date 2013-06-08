;;;; Package evil-sexp
(sm-package evil-sexp
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/evil-sexp"))

(require 'evil-sexp)

(define-key evil-motion-state-map (kbd "M-j") 'evil-enter-sexp)
(define-key evil-motion-state-map (kbd "M-k") 'evil-exit-sexp)
(define-key evil-motion-state-map (kbd "M-h") 'evil-backward-sexp)
(define-key evil-motion-state-map (kbd "M-l") 'evil-forward-sexp)

(sm-provide :package evil-sexp)
