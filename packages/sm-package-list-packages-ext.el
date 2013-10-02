;;;; Package list-packages-ext
(sm-package list-packages-ext
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (local-repo "list-packages-ext"))

(require 'list-packages-ext)

(add-hook 'package-menu-mode-hook '(lambda ()
                                    (list-packages-ext-mode 1)
                                    (hl-line-mode 1)))

;;;;  Kludges

;; (eval-after-load 'smooth-scrolling
;;   (progn
;;     (defun disable-smooth-scroll ()
;;       (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
;;       (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
;;       (ad-activate 'next-line)
;;       (ad-activate 'previous-line))


;;     (add-hook 'list-packages-ext-mode-hook
;;               'disable-smooth-scroll)))



(sm-provide :package list-packages-ext)
