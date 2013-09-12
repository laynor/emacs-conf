;;;; Package quickrun
(sm-package quickrun
            :package-manager "package"
            :unmanaged-p nil)

(quickrun-add-command "js/d8"
                      '((:command . "d8")
                        (:exec    . ("%c %s")))
                      :default "js")

(quickrun-set-default "js3" "js/d8")
(quickrun-set-default "js" "js/d8")
(quickrun-set-default "javascript" "js/d8")

(sm-provide :package quickrun)
