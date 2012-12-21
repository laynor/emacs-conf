;;;; Module python
(sm-module python
           :unmanaged-p nil
           :require-packages '(auto-complete jedi))

(sm-module-pre (python)
  )

(sm-module-post (python)
  (add-hook 'python-mode-hook 'jedi:setup)
  )

(sm-provide :module python)
