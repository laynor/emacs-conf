;;;; Module clojure
(sm-module clojure
           :unmanaged-p nil
           :require-packages '("cider" "ac-cider-compliment"))

(sm-module-pre (clojure)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (clojure)
  ;; TODO insert your post-package-initialization code here
  (defun turn-off-sublimity-mode ()
    (sublimity-mode -1))
  (add-hook 'cider-repl-mode-hook 'turn-off-sublimity-mode)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

  )

(sm-provide :module clojure)
