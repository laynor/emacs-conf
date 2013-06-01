;;;; Package direx
(sm-package direx
            :package-manager "package"
            :unmanaged-p nil)

(require 'direx)
(defun popwin:direx (dirname)
  "Edit file FILENAME with popup window by `popwin:popup-buffer'."
  (interactive
   (list (ido-read-directory-name "Direx (popup): ")))
  (popwin:popup-buffer (direx:find-directory-noselect dirname) :position 'left))

(sm-provide :package direx)
