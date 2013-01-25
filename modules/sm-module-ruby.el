;;;; Module ruby
(sm-module ruby
           :unmanaged-p nil
           :require-packages '(yasnippet rsense enotify enotify-espectator hideshow ale-fixme))

(sm-module-pre (ruby)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (ruby)
  ;; Folding with hideshow
  (add-to-list 'hs-special-modes-alist
               '(ruby-mode
                 "\\(class\\|def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
                 (lambda (arg) (ruby-end-of-block)) nil))
  ;; Autocomplete with rsense
  (add-hook 'ruby-mode-hook 'add-rsense-ac-sources)
  (add-hook 'ruby-mode-hook 'add-yasnippet-ac-sources)
  (add-hook 'ruby-mode-hook 'yas-minor-mode-on)
  (add-hook 'ruby-mode-hook 'yas-reload-all)
  ;; todo/fixme comments
  (add-hook 'ruby-mode-hook '(lambda () (fic-ext-mode 1)))
  )

(sm-provide :module ruby)
