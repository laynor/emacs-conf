;;;; Module python
(sm-module python
           :unmanaged-p nil
           :require-packages '(auto-complete jedi jedi-eldoc flymake-cursor flycheck))

(sm-module-pre (python)
  )

(sm-module-post (python)
  ;; Define a flake8 checker that doesn't get mad about several
  ;; coding-convention related things.
  (defvar flake8-ignored-codes '("E501" ; Line too long
                                 "E201" ; Whitespace after (
                                 "E202" ; Whitespace before )
                                 ))

  (defvar flycheck-checker-python-flake8-with-excludes
    `(:command ("flake8" ,(concat "--ignore=" (mapconcat 'identity flake8-ignored-codes ",")) source-inplace)
               :error-patterns
               (("^\\(.*?\\):\\([0-9]+\\):\\([0-9]*\\):? \\(E.*\\)$" 1 2 3 4 error)
                ("^\\(.*?\\):\\([0-9]+\\):\\([0-9]*\\):? \\(W.*\\)$" 1 2 3 4 warning))
             :modes python-mode))

  (add-hook 'python-mode-hook 'flycheck-mode)

  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook (lambda () (setq popup-max-menu-width 0.5)))
  (add-hook 'python-mode-hook 'jedi-eldoc-mode)
  )

(sm-provide :module python)
