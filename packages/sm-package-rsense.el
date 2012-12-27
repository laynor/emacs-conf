;;;; Package rsense
(sm-package rsense
            :package-manager "package"
            :unmanaged-p nil)

(setq rsense-home (expand-file-name "~/local/opt/rsense-0.3"))

(require 'rsense)

(defun add-rsense-ac-sources()
  (interactive)
  (add-to-list 'ac-sources 'ac-source-rsense-method)
  (add-to-list 'ac-sources 'ac-source-rsense-constant))

(sm-provide :package rsense)
