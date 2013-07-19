;;;; Package xgtags-extension
(sm-package xgtags-extension
            :package-manager nil
            :unmanaged-p t)

(require 'xgtags-extension)
(define-key xgtags-select-mode-map (kbd "n") 'xgtags-select-next-tag-line-show)
(define-key xgtags-select-mode-map (kbd "p") 'xgtags-select-prev-tag-line-show)


(sm-provide :package xgtags-extension)
