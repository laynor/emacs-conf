;;;; Package erc
(sm-package erc
            :package-manager "builtin"
            :unmanaged-p nil)

(defun erc-autojoin-enable (&rest args) (interactive) nil)

(load "~/Dropbox/emacs/.ercpass")
(require 'erc)
(require 'erc-join)
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)

(setq erc-nick '("ale`" "ale``"))

(setq erc-nickserv-passwords
      `((freenode (("ale`" . ,freenode-passwd-laynor)
		   ("ale``" . ,freenode-passwd-laynor)))))
(defun erc-freenode-laynor ()
  (interactive)
  (erc :server "irc.freenode.net" :port 8001 :nick "ale`"))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
;; The ~/.ercpass file contains a line like this one:

;; (setq freenode-passwd-laynor "myfreenodepassword")
(setq erc-modules '(autojoin button completion dcc fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp spelling track))
(erc-update-modules)

(sm-provide :package erc)
