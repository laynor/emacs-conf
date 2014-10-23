;;;; This module should contain your basic configuration, that will be
;;;; shared by all the profiles - if they include the base module, of
;;;; course!

(sm-module "base"
           ;; add the packages required by your basic configuration here
  :require-packages '( "ag"
                       "ace-jump-mode"
                       "auto-complete"
                       "browse-kill-ring"
                       "charmap"
                       "diff-hl"
                       "diminish"
                       "dired"
                       "direx"
                       "dollaro"
                       "erc"
		       "f"
                       "undo-tree"
                       "paredit"
		       "el-get"
                       "evil"
                       "evil-sexp"
		       "expand-region"
                       "flx"
                       "flx-ido"
                       "fuzzy"
                       ;;"gist"
                       "gitignore-mode"
		       "goto-page"
                       "guide-key"
		       ;;"helm"
                       "ht"
                       "hide-comnt"
		       "highlight-escape-sequences"
                       "httpcode"
                       ;;"ido-hacks"
                       "ido-ubiquitous"
		       "ido-vertical-mode"
                       "ipa"
                       "magit"
                       "magit-filenotify"
                       "git-commit-mode"
                       "git-messenger"
                       "markdown-mode"
                       "markdown-mode+"
                       ;;"melpa"
                       "mode-icons"
                       ;;"package"
		       "page-move"
                       ;;"parenface"
		       "pcache"
		       "persistent-soft"
                       "popup-git"
                       "powerline"
                       "pp-c-l"
                       ;; "pretty-symbols-mode"
                       "projectile"
                       "quickrun"
                       "s"
                       "list-packages-ext"
                       "melpa-upstream-visit"
                       "rainbow-delimiters"
                       "readline-complete"
                       "shell-pop"
                       "smex"
                       "surround"
                       "switch-window"
                       ;; "smooth-scrolling"
                       "wgrep"
                       "whitespace"
                       "woman"
                       ;;"undohist"
                       "wgrep-ack"
                       "yalinum"
                       )
           :unmanaged-p nil)


(sm-module-pre (base)
  (ido-mode t)
  )


(sm-module-post (base)
  
;;;; -- Scrolling --
  ;; (setq scroll-step 1)
  ;; (setq scroll-conservatively 1000)
  ;; (setq auto-window-vscroll nil)

  
;;;; -- Smotitah aliases --
  (defalias 'em 'sm-edit-module)
  (defalias 'ep 'sm-edit-package)
  (defalias 'epr 'sm-edit-profile)


;;;; -- Key Bindings --

  ;;; F Keys
  (global-set-key [(f7)] 'magit-status)
  (global-set-key [(f8)] 'goto-page)
  (global-set-key [(f9)] 'open-notes-file)

  ;;; Alias C-. to M-TAB
  (define-key key-translation-map (kbd "C-.") (kbd "M-TAB"))

  ;;; Elisp testing
  (global-set-key (kbd "C->") 'message-point)

  ;;; Buffer/window navigation
  (global-set-key (kbd "C-x o") 'switch-window)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  ;;; Editing
  (global-set-key (kbd "C-c e") 'eval-and-replace)


;;;; -- Random configuration --

  ;;; Frame title
  (setq frame-title-format '(buffer-file-name "%b - emacs" ("%b - emacs")))

  ;;; AUTO-MODES
  (add-to-list 'auto-mode-alist (cons "\\.zsh$" 'shell-script-mode))

  ;;; Unique buffer names
  (require 'uniquify)

  ;;; let M-{b,f} move by subwords
  (global-subword-mode 1)

  ;;; - before-save-hook -
  ;;; remove trailing whitespace
  (add-hook 'before-save-hook (lambda () (unless (or (ignore-errors makefile-mode)
                                                     (memq major-mode '(ipa-mode
                                                                        makefile-mode)))
                                           (delete-trailing-whitespace))))

;;;;  Custom commands

  ;;; Comments
  (defun insert-title (fill title)
    (interactive "cFill with: \nsEnter string:")
    (let* ((l (length title))
           (n (/ (- 80 l) 2))
           (title (concat " " title " ")))
      (dotimes (i (floor n))
        (insert fill))
      (insert title)
      (dotimes (i (ceiling n))
        (insert fill))))

  ;; insert titled comment
  (defvar *titled-comment-length* 90
    "Total width of comments inserted with `insert-titled-comment'")

  (defun insert-titled-comment (string)
    "Inserts a comment in the stile
  ;;;; ------------------------------ STRING expansion -------------------------------
The number of dashes is calculated based on `*titled-comment-length*'.
"
    (interactive "sTitle: ")
    (let* ((clen (length string))
           (comment-prefix (format "%s " (s-repeat 4 comment-start)))
           (remaining-space (- *titled-comment-length* (length comment-prefix) (+ 2 clen)))
           (dashes-left (s-repeat (floor (/ remaining-space 2.0)) "-"))
           (dashes-right (s-repeat (ceiling (/ remaining-space 2.0)) "-")))
      (insert (format "%s%s %s %s\n" comment-prefix dashes-left string dashes-right))))


  ;;; Lisp interaction
  (defun eval-and-replace (&optional arg)
    "Replace the preceding sexp with its value."
    (interactive "P")
    (backward-kill-sexp)
    (let ((print-fn (if arg 'princ 'prin1)))
      (condition-case nil
	  (funcall print-fn (eval (read (current-kill 0)))
		   (current-buffer))
	(error (message "Invalid expression")
	       (insert (current-kill 0))))))

  ;;; Elisp testing
  (defun message-point()
    "Display the value of (point) in the minibuffer."
    (interactive)
    (message "Point: %S" (point)))

  (defun length-of-region(start end)
    "Places the length of the current region in the kill ring."
    (interactive "r")
    (kill-new (message "%S" (- end start))))

  ;;; Random stuff
  (defun open-notes-file ()
    (interactive)
    (find-file-other-window "~/.emacs.d/notes.org"))
  )

(sm-provide :module base)
;;;; End base module
