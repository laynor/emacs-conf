;;;; Package ale-testing
(sm-package ale-testing
            :package-manager nil
            :unmanaged-p t)

(defun tt/syntax-at-point (&optional arg)
  (interactive)
  (let ((syntax (char-syntax (char-after (or arg (point))))))
    (when (called-interactively-p)
      (message "Syntax at point: %S" (string syntax)))
    syntax))

(sm-provide :package ale-testing)
