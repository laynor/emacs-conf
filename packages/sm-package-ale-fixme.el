;;;; Package fic-ext
(sm-package ale-fixme
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))

(defun get-same-mode-buffers ()
  "Get all the buffers having the same Major mode as the current
buffer"
  (interactive)
  (remove-if-not (lambda (buf)
		   (eql (with-current-buffer buf major-mode)
			major-mode))
		 (buffer-list)))

(defun fixme-occur (&optional arg)
  "Finds all the occurrences of the fixme keywords in all buffers
having the same major mode as the current buffer"
  (interactive "P")
  (multi-occur (get-same-mode-buffers)
	       (if arg
                   (read-string "Search in same mode buffers: ")
                 (fic-search-re))))


(require 'fic-ext-mode)

(defun turn-on-fixme-mode ()
  (fic-ext-mode 1))

(setq fic-highlighted-words '("FIXME" "TODO" "BUG" "KLUDGE" "XXX" "UGLY"))

(sm-provide :package ale-fixme)
