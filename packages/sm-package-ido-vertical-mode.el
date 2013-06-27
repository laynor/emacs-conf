;;;; Package ido-vertical-mode
(sm-package ido-vertical-mode
            :package-manager "package"
            :unmanaged-p nil)

(ido-vertical-mode t)
(defadvice ido-completions (after smufu (name) activate)
  (let* ((n (length (split-string ad-return-value "\n" t))))
    (setq ad-return-value (concat ad-return-value (s-repeat (1+ (- ido-max-prospects n)) "\n")))))

(sm-provide :package ido-vertical-mode)
