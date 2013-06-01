;;;; Module python
(sm-module python
           :unmanaged-p nil
           :require-packages '(auto-complete jedi flycheck ale-fixme enotify yasnippet))

(sm-module-pre (python)
  )

(sm-module-post (python)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook (lambda () (setq popup-max-menu-width 0.5)))
  (add-hook 'python-mode-hook 'yas-minor-mode)
  )

(sm-provide :module python)
