;;;; Package magit-filenotify
(sm-package magit-filenotify
            :package-manager "package"
            :unmanaged-p nil)

;;; TODO insert your package initialization code here
(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

(sm-provide :package magit-filenotify)
