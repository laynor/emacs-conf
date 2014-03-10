;;;; Package diff-hl
(sm-package diff-hl
            :package-manager "package"
            :unmanaged-p nil)

(sm-integrate-with magit
    (defun magit-refresh-project-buffers ()
      (let* ((top-dir (magit-get-top-dir))
             (buffers (remove-if-not (lambda (buf)
                                       (let ((buf-fname (buffer-file-name buf)))
                                         (and buf-fname
                                              (string= top-dir
                                                       (magit-get-top-dir
                                                        (file-name-directory
                                                         buf-fname))))))
                                     (buffer-list))))
        (dolist (buf buffers)
          (with-current-buffer buf
            (when diff-hl-mode
              (diff-hl-update))))))
  (defadvice magit-refresh (after diff-hl-refresh-after-magit-refresh activate)
    (magit-refresh-project-buffers)))

(sm-provide :package diff-hl)
