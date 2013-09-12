;;;; Package ac-slime
(sm-package ac-slime
            :package-manager "package"
            :unmanaged-p nil)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(sm-provide :package ac-slime)
