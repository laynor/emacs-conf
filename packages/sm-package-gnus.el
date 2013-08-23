;;;; Package gnus
(sm-package gnus
            :package-manager nil
            :unmanaged-p t)

;; (setq gnus-select-method '(nntp "news.gmane.org"))
(setq gnus-select-methods '((nnimap "tekno-soft"
				    (nnimap-address "mail.tekno-soft.it")
				    (nnimap-server-port 993)
				    (nnimap-authenticator login)
				    (nnimap-expunge-on-close 'never)
				    (nnimap-stream starttls))))

(sm-provide :package gnus)
