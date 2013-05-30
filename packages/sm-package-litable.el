;;;; Package litable
(sm-package litable
            :package-manager "package"
            :unmanaged-p nil)

(defun insert-closed-paren (&optional n)
  (interactive "p")
  (self-insert-command n)
  (save-excursion
    (backward-char)
    (litable-refresh)))

(add-hook litable-mode-hook
          (lambda ()
            (local-set-key (kbd ")") 'insert-closed-paren)))

(sm-provide :package litable)
