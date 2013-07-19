;;;; Package xgtags
(sm-package xgtags
            :package-manager nil
            :unmanaged-p t)

(require 'xgtags)

(defun xgtags-auto-update ()
  (if (and xgtags-mode gtags-auto-update buffer-file-name)
      (progn
	(gtags-push-tramp-environment)
	(call-process gtags-global-command nil nil nil "-u" (concat "--single-update=" (gtags-buffer-file-name)))
	(gtags-pop-tramp-environment))))

(add-hook 'after-save-hook 'xgtags-auto-update)

(defun turn-on-xgtags-mode ()
  (xgtags-mode 1))

(define-key xgtags-mode-map (kbd "C-;") 'xgtags-find-rtag)

(define-key xgtags-mode-map (kbd "M-p")
  'xgtags-select-prev-tag)
(define-key xgtags-mode-map (kbd "M-n")
  'xgtags-select-next-tag)

(sm-integrate-with evil

  (evil-define-motion evil-jump-to-xgtags-tag (arg)
    "Jump to tag under point.
If called with a prefix argument, provide a prompt
for specifying the tag."
    :jump t
    (interactive "P")
    (call-interactively #'xgtags-find-tag))
  (evil-define-key 'normal c-mode-map (kbd "C-]")
    'evil-jump-to-xgtags-tag)
  (evil-define-key 'normal 'c-mode-map (kbd "C-t")
    'xgtags-pop-stack))






(sm-provide :package xgtags)
