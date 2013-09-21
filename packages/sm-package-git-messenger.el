;;;; Package git-messenger
(sm-package git-messenger
            :package-manager "package"
            :unmanaged-p nil)

(require 'git-messenger)

(defun git-messenger:commit-message (commit-id)
  (with-temp-buffer
    (let ((cmd (git-messenger:cat-file-command commit-id)))
      (unless (zerop (call-process-shell-command cmd nil t))
        (error "Failed: %s" cmd))
      (goto-char (point-min))
      (let* ((author-start (re-search-forward "^author "))
             (author-end (search-forward ">"))
             (author (buffer-substring-no-properties author-start author-end))
             (commit-time-end (re-search-forward "[[:digit:]]+ "))
             (commit-time (buffer-substring-no-properties author-end commit-time-end))
             (date (destructuring-bind (sec minute hour day month year dow dst zone)
                       (decode-time (seconds-to-time 1362757615))
                     (format "%s-%0.2d-%0.2d %0.2d:%0.2d:%0.2d" year month day hour minute sec))))
         (forward-paragraph)
         (format "Commit:\t%s\nAuthor:\t%s\nDate:\t%s\n%s"
                 commit-id
                 author
                 date
                 (buffer-substring-no-properties (point) (point-max)))))))

(define-key prog-mode-map (kbd "C-c g") 'git-messenger:popup-message)

(sm-provide :package git-messenger)
