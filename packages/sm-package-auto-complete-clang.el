;;;; Package auto-complete-clang
(sm-package auto-complete-clang
            :package-manager "package"
            :unmanaged-p nil)

(require 'auto-complete-clang)

(makunbound 'ac-source-clang)

(ac-define-source clang
  '((candidates . ac-clang-candidate)
    (candidate-face . ac-clang-candidate-face)
    (selection-face . ac-clang-selection-face)
    (prefix . ac-clang-prefix)
    (summary . ac-clang-document)
    (requires . 0)
    (document . ac-clang-document)
    (action . ac-clang-action)
    (cache)
    (symbol . "c")))

(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)


(sm-provide :package auto-complete-clang)
