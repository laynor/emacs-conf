;;;; Package wanderlust
(sm-package wanderlust
            :package-manager "el-get"
            :unmanaged-p nil)

(setq elmo-imap4-default-server "mail.tekno-soft.it"
      elmo-imap4-default-port 993
      elmo-imap4-default-stream-type 'starttls
      elmo-imap4-default-authenticate-type 'login
      elmo-imap4-default-user "alessandro@tekno-soft.it"
      elmo-imap4-use-modified-utf7 t)

(setq wl-mime-charset 'utf-8)

(require 'wl)
(setq wl-thread-have-younger-brother-str (if wl-on-mule "┣" "+"))
(setq wl-thread-youngest-child-str       (if wl-on-mule "┗" "+"))
(setq wl-thread-vertical-str             (if wl-on-mule "┃" "|"))
(setq wl-thread-horizontal-str           (if wl-on-mule "━" "-"))
(setq wl-thread-space-str                (if wl-on-mule "　" " "))

(sm-provide :package wanderlust)
