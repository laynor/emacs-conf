;;;; Package goto-page
(sm-package goto-page
            :package-manager nil
            :unmanaged-p t)

(defun next-page (&optional count)
  (forward-page count)
  (and (/= (point) (point-max))
       (point)))

(defmacro* dopage ((page-identifier &optional index) &body body)
  (let ((page-id-beg (gensym))
        (page-id-end (gensym))
        (buffer-end (gensym))
        (idx (or index (gensym)))
        (next-page-pos (gensym))
        (next-page (gensym)))
    `(flet ((,next-page (&optional count)
                        (forward-page count)
                        (and (/= (point) (point-max))
                             (point))))
       (save-excursion
         (goto-char (point-min))
         (do* ((,next-page-pos (next-page) (next-page))
               (,idx 0 (1+ ,idx)))
             ((not ,next-page-pos))
           (let* ((,page-id-beg (line-beginning-position 2))
                  (,page-id-end (line-end-position 2))
                  (,page-identifier (buffer-substring-no-properties ,page-id-beg ,page-id-end)))
             ,@body))))))

(defun page-pos (page-id matchp)
  (block foo
    (dopage (pid)
            (when (funcall matchp page-id pid)
              (return-from foo (line-beginning-position 2))))
    nil))


(setq goto-page-bob-identifier (propertize "BEGINNING" 'goto-page-pos 'point-min 'face 'success))

(setq goto-page-eob-identifier (propertize "END" 'goto-page-pos 'point-max 'face 'error))

(defun* goto-page (page-id &optional (matchp 'string-match-p))
  (interactive  (list (let ((bpi (buffer-page-identifiers)))
                        (if bpi
                            (ido-completing-read "Goto Page: " bpi nil t)
                          (error "No pages on this buffer")))))
  (let ((pos (or (ignore-errors (funcall (get-text-property 0 'goto-page-pos page-id)))
                 (page-pos page-id matchp))))
    (when pos
      (goto-char pos))))


(defun* buffer-page-identifiers (&optional buffer)
  (let ((buf (or buffer (current-buffer)))
        id-list)
    (with-current-buffer buf
      (push goto-page-bob-identifier id-list)
      (dopage (page-id)
              (push page-id id-list))
      (push goto-page-eob-identifier id-list))
    (reverse id-list)))

(sm-provide :package goto-page)
