;;;; Package ipa
(sm-package ipa
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/ipa"))

(require 'ipa)

(evil-global-set-key 'normal (kbd "M-i M-i") 'ipa-toggle)
(evil-global-set-key 'normal (kbd "M-i i") 'ipa-insert)
(evil-global-set-key 'normal (kbd "M-i e") 'ipa-edit)
(evil-global-set-key 'normal (kbd "M-i m") 'ipa-move)

(defface ipa-face
  '((((class color))
     (:foreground "black" :background "thistle3" :weight bold))
    (t (:weight bold)))
  "face to fontify Enotify Success messages"
  :group 'enotify)

;; REDEFINED!
;; Modified to support multi-line string indenting
(defun ipa-set-overlay-text-above (overlay text)
  (if (string-match ipa-annotation-id-regexp text)
      (setq text (match-string 2 text)))
  (save-excursion
    (let ((ipa-indent-level (current-indentation)))
      (beginning-of-line)
      (let ((text (mapconcat #'identity (split-string text "\n")
                             (concat "\n" (string-repeat " " (+ 2 ipa-indent-level))))))
        (overlay-put overlay 'before-string
                     (if (equal text "") ""
                       (propertize
                        (concat
                         (string-repeat " " ipa-indent-level) "* " text "\n")
                        'face ipa-annotation-face)))))))

(sm-provide :package ipa)
