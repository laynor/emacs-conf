;;;; Package jedi
(sm-package jedi
            :package-manager "package"
            :unmanaged-p nil)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
;; (setq jedi:tooltip-method '(pos-tip))

(autoload 'jedi:setup "jedi" nil t)

(sm-provide :package jedi)
