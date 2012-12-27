;;;; Package jedi
(sm-package jedi
            :package-manager "package"
            :unmanaged-p nil)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
;; (setq jedi:tooltip-method '(pos-tip))

(autoload 'jedi:setup "jedi" nil t)

(defadvice jedi:ac-direct-matches (after jedi:remove-summary activate)
  (setq ad-return-value
        (mapcar (lambda (item)
                  (propertize item
                              'summary nil))
                ad-return-value)))

(sm-provide :package jedi)
