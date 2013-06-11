;;;; Module mac
(sm-module mac
           :unmanaged-p nil
           :require-packages '())

(sm-module-pre (mac)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (mac)
  ;; (setq mac-option-modifier 'super)
  ;; (setq mac-command-modifier 'meta)
  ;; (setq ns-function-modifier 'hyper)
  (setq mac-allow-anti-aliasing t)

  ;; Move to trash when deleting stuff
  (setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")
  )

(sm-provide :module mac)
