;;;; Package woman
(sm-package woman
            :package-manager nil
            :unmanaged-p t)

(require 'woman)

(defun woman-other-window (&optional topic re-cache)
  (interactive (list nil current-prefix-arg))
  (if (or (not (stringp topic)) (string-match "\\S " topic))
      (let ((file-name (woman-file-name topic re-cache)))
	(if file-name
            (progn
              (when (= (length (window-list)) 1)
                (split-window nil nil t))
              (other-window 1)
              (woman-find-file file-name)
              (other-window -1))
	  (message
	   "WoMan Error: No matching manual files found in search path")
	  (ding)))
    (message "WoMan Error: No topic specified in non-interactive call")
    (ding)))

(sm-provide :package woman)
