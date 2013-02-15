;;;; Module ruby
(sm-module ruby
           :unmanaged-p nil
           :require-packages '(yasnippet rsense enotify
                               highlight-indentation
                               hideshow
                               ale-fixme rspec-mode markdown-mode
                               markdown-mode+ yard-mode
                               rdoc-mode))

(sm-module-pre (ruby)
  ;; Add current ruby elisp directory to load-path
  (require 'ruby-mode)
  (add-to-list 'load-path (concat (getenv "HOME") "/.rvm/src/" (getenv "rvm_ruby_string") "/misc/"))
  )

(sm-module-post (ruby)
  ;; (require 'ruby-electric)
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
  (add-hook 'ruby-mode-hook #'(lambda () (fic-ext-mode 1)))
  (add-hook 'ruby-mode-hook #'(lambda () (highlight-indentation-mode 1)))
  (add-hook 'ruby-mode-hook #'yard-turn-on)
  ;; Auto indent current line when pressing RET
  (defun indent-then-newline-and-indent()
    (interactive)
    (indent-according-to-mode)
    (newline-and-indent))
  (define-key ruby-mode-map (kbd "RET") 'indent-then-newline-and-indent)
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  )

(sm-provide :module ruby)
