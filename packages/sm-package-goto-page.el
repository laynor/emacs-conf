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

(defun* goto-page (page-id &optional (matchp 'string-match-p))
  (interactive  (list (ido-completing-read "Goto Page: " (buffer-page-identifiers) nil t)))
  (let ((pos (page-pos page-id matchp)))
    (when pos
      (goto-char pos))))


(defun* buffer-page-identifiers (&optional buffer)
  (let ((buf (or buffer (current-buffer)))
	id-list)
    (with-current-buffer buf
      (dopage (page-id)
	      (push page-id id-list)))
    (reverse id-list)))

(sm-provide :package goto-page)
