;;;; Module erlang
(sm-module erlang
           :unmanaged-p nil
           :require-packages '(erlang distel auto-complete auto-complete-distel))

(sm-module-pre (erlang)
  )

(sm-module-post (erlang)
  (add-hook 'erlang-mode-hook 'ac-distel-setup)
  (add-hook 'erlang-shell-mode-hook 'ac-distel-setup)
  )

(sm-provide :module erlang)
