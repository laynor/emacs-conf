;;;; Package elisp-utils
(sm-package elisp-utils
            :package-manager nil
            :unmanaged-p t)

(defun display-prefix (arg)
  "Display the value of the raw prefix arg."
  (interactive "p")
  (message "%s" arg))


;;; TODO insert your package initialization code here

(sm-provide :package elisp-utils)
