;;;; Package diminish
(sm-package diminish
            :package-manager "package"
            :unmanaged-p nil)

(require 'diminish)

(sm-integrate-with projectile
  (diminish 'projectile-mode))

(sm-integrate-with guide-key
  (diminish 'guide-key-mode))

(sm-integrate-with hideshow
  (diminish 'hs-minor-mode))

(sm-integrate-with fic-ext-mode
  (diminish 'fic-ext-mode))

(sm-integrate-with undo-tree
  (diminish 'undo-tree-mode))

(sm-integrate-with elisp-slime-nav
  (diminish 'elisp-slime-nav-mode))

(sm-integrate-with subword
  (diminish 'subword-mode))

(sm-integrate-with abbrev
  (diminish 'abbrev-mode))

(sm-provide :package diminish)
