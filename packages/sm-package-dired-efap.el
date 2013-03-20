;;;; Package dired-efap
(sm-package dired-efap
            :package-manager "package"
            :unmanaged-p nil)

(require dired-efap)
(evil-define-key 'normal dired-mode-map (kbd "C-c C-c") 'dired-efap)

(sm-provide :package dired-efap)
