;;;; Module python
(sm-module python
           :unmanaged-p nil
           :require-packages '(auto-complete jedi jedi-eldoc flycheck ale-fixme))

(sm-module-pre (python)
  )

(sm-module-post (python)
  ;; Define a flake8 checker that doesn't get mad about several
  ;; coding-convention related things.
  (defvar flake8-ignored-codes '("E501" ; Line too long
                                 "E201" ; Whitespace after (
                                 "E202" ; Whitespace before )
                                 ))

  (flycheck-declare-checker python-flake8-with-excludes
    "A Python syntax and style checker using the flake8 utility, with several ignored codes.
   See http://pypi.python.org/pypi/flake8."
    :command `("flake8" ,(concat "--ignore=" (mapconcat 'identity flake8-ignored-codes ",")) (config "--config" flycheck-flake8rc) source-inplace)
    :error-patterns
    '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:[[:alpha:]]\\{2\\}.*\\)$" error)
      ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:E[0-9]+.*\\)$"
       error)
      ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?:\\(?3:[0-9]+\\):\\)? \\(?4:W[0-9]+.*\\)$"
       warning))
    :modes 'python-mode)

  (add-to-list 'flycheck-checkers 'python-flake8-with-excludes)



  (add-hook 'python-mode-hook 'flycheck-mode)

  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook (lambda () (setq popup-max-menu-width 0.5)))
  (add-hook 'python-mode-hook 'jedi-eldoc-mode)
  (add-hook 'python-mode-hook 'turn-on-fixme-mode)
  )

(sm-provide :module python)
