;;;; Package bbdb
(sm-package bbdb
            :package-manager "package"
            :unmanaged-p nil)


(require 'bbdb)
(bbdb-initialize 'gnus 'message)

(load "bbdb-com")

(load "bbdb-gnus")


(load "bbdb-message")

(global-set-key (kbd "C-c b") 'bbdb)


(sm-provide :package bbdb)
