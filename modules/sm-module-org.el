;;;; Module org
(sm-module org
           :unmanaged-p nil
           :require-packages '(org-bullets))

(sm-module-pre (org)
  )

(sm-module-post (org)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(sm-provide :module org)
