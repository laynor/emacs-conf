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
		   ("ale``" . ,freenode-passwd-laynor)))
        (BitlBee (("ale`" . ,bitlbee-passwd-laynor)))))


(defun bitlbee-netrc-identify ()
  "Auto-identify for Bitlbee channels using authinfo or netrc.

The entries that we look for in netrc or authinfo files have
their 'port' set to 'bitlbee', their 'login' or 'user' set to
the current nickname and 'server' set to the current IRC
server's name. A sample value that works for authenticating
as user 'USER' on server 'localhost' is:

machine localhost port bitlbee login USER password PASSWORD"
  (interactive)
  (when (string= (buffer-name) "&bitlbee")
    (let* ((secret (plist-get (nth 0 (auth-source-search :max 1
                                                         :host erc-server
                                                         :user (erc-current-nick)
                                                         :port "bitlbee"))
                              :secret))
           (password (if (functionp secret)
                         (funcall secret)
                         secret)))
      (erc-message "PRIVMSG" (concat (erc-default-target) " " "identify" " " password) nil))))

;; Enable the netrc authentication function for &biblbee channels.
(add-hook 'erc-join-hook 'bitlbee-netrc-identify)


(defun erc-freenode-laynor ()
  (interactive)
  (erc :server "irc.freenode.net" :port 8001 :nick "ale`"))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
;; The ~/.ercpass file contains a line like this one:

;; (setq freenode-passwd-laynor "myfreenodepassword")
;; (setq erc-modules '(autojoin completion dcc fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp spelling track))
;; (erc-update-modules)

(sm-provide :package erc)
