;;;; Module elisp
(sm-module elisp
           :unmanaged-p nil
           :require-packages '("elisp-slime-nav" "ale-fixme" "auto-complete" "hl-sexp" "highlight-cl"
                               "litable" "ale-testing" "rainbow-mode" "edit-color-stamp" "yasnippet" "flash-region"))

(sm-module-pre (elisp)
  ;; TODO insert your pre-package-initialization code here
  )

(sm-module-post (elisp)

  ;;; REPL
  (defun visit-ielm ()
    "Create or visit a `ielm' buffer."
    (interactive)
    (let ((ielm-buffer (get-buffer "*ielm*")))
      (unless (eq (current-buffer) ielm-buffer)
        (if ielm-buffer
            (switch-to-buffer-other-window ielm-buffer)
          (split-window-sensibly (selected-window))
          (other-window 1)
          (ielm)))))

(defun eval-defun-flash-region (edebugit)
  (interactive "P")
  (eval-defun edebugit)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (flash-region (car bounds) (cdr bounds) 'region 0.25)))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun-flash-region)
  (global-set-key [(f5)] 'visit-ielm)

  ;; Activate minor modes
  (defun my-emacs-lisp-mode-hook ()
    (turn-on-eldoc-mode)
    (ac-emacs-lisp-mode-setup)
    (hl-sexp-mode 1)
    (highlight-cl-add-font-lock-keywords)
    (elisp-slime-nav-mode 1)
    (ignore-errors (yas-minor-mode-on))
    (add-hook 'local-write-file-hooks
              'check-parens))

  (add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)
  (add-hook 'lisp-interaction-mode-hook 'my-emacs-lisp-mode-hook)

  (sm-integrate-with (:package rainbow-mode)
      (add-hook 'lisp-interaction-mode-hook #'(lambda () (rainbow-mode t)))
    (add-hook 'emacs-lisp-mode-hook #'(lambda () (rainbow-mode t))))

 ;;;;;;;;;;;;;;;;;;;;;
 ;;; evil integration
  (sm-integrate-with (:package evil)
      (evil-define-key 'normal emacs-lisp-mode-map (kbd "M-.")
                       'elisp-slime-nav-find-elisp-thing-at-point))


  (setq lisp-indent-function 'common-lisp-indent-function)

  (put 'if 'common-lisp-indent-function 2)

  (put 'cl-flet 'common-lisp-indent-function
       (get 'flet 'common-lisp-indent-function))

  (put 'gv-define-setter 'common-lisp-indent-function
       (get 'defmacro 'common-lisp-indent-function))

  (font-lock-add-keywords 'emacs-lisp-mode '(("\\<\\(cl-\\(flet\\|labels\\)\\)\\>" . font-lock-keyword-face)))


  ;; (add-hook 'emacs-lisp-mode-hook '(lambda () (pretty-symbols-mode 1)))

  ;; (evil-define-motion evil-forward-sexp (count)
  ;;   (forward-sexp count))
  ;; (evil-define-motion evil-forward-sexp (count)
  ;;   "Moves the cursor one sexp forward."
  ;;     (dotimes (i 1)
  ;;       (when (= (char-after  (point)) ?\n)
  ;;         (re-search-forward "[^\\\\])"))
  ;;       (cond ((= (char-after) ?\))
  ;;              (goto-char (+ 2 (point))))
  ;;             (t (forward-sexp)))))

  ;; (evil-define-motion evil-backward-sexp (count)
  ;;   (backward-sexp count))

  ;; (evil-define-motion evil-backward-sexp (count)
  ;;   "Moves the cursor one sexp backward."
  ;;     (dotimes (i 1)
  ;;       (while (= (char-before) ?\n)
  ;;         (ignore-errors (re-search-backward ")"))
  ;;         (forward-char))
  ;;       (cond ((= (char-before) ?\()
  ;;              (goto-char (1- (point))))
  ;;             (t (backward-sexp)))))


  ;; (evil-define-key 'normal lisp-interaction-mode-map
  ;;   (kbd "M-j") 'down-list
  ;;   (kbd "M-k") 'up-list
  ;;   (kbd "M-l") 'evil-forward-sexp
  ;;   (kbd "M-h") 'evil-backward-sexp)
  ;; (evil-define-key 'normal emacs-lisp-mode-map
  ;;   (kbd "M-j") 'down-list
  ;;   (kbd "M-k") 'up-list
  ;;   (kbd "M-l") 'evil-forward-sexp
  ;;   (kbd "M-h") 'evil-backward-sexp)
                                        ; could be bad, will not let you save at all, until you correct the error
 ;;; End module post
  )


;; (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode-on)
(sm-provide :module elisp)
