;;;; Module ruby
(sm-module ruby
           :unmanaged-p nil
           :require-packages '(yasnippet rsense enotify enotify-espectator))

(sm-module-pre (ruby)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (ruby)
  (add-hook 'ruby-mode-hook 'add-rsense-ac-sources)
  (add-hook 'ruby-mode-hook 'add-yasnippet-ac-sources)
  (add-hook 'ruby-mode-hook 'yas-minor-mode-on)
  (add-hook 'ruby-mode-hook 'yas-reload-all)
  )

(sm-provide :module ruby)
