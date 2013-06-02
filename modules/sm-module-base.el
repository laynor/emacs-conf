;;;; This module should contain your basic configuration, that will be
;;;; shared by all the profiles - if they include the base module, of
;;;; course!

(sm-module "base"
           ;; add the packages required by your basic configuration here
  :require-packages '( "ag"
                       "ace-jump-mode"
                       "auto-complete"
                       "browse-kill-ring"
                       "diff-hl"
                       "diminish"
                       "dired"
                       "direx"
                       "erc"
                       "undo-tree"
                       "evil"
                       "flx"
                       "fuzzy"
                       "gist"
                       "gitignore-mode"
                       "guide-key"
                       "hide-comnt"
                       "httpcode"
                       "ido-hacks"
                       "ido-ubiquitous"
                       "ipa"
                       "magit"
                       "git-commit-mode"
                       "markdown-mode"
                       "markdown-mode+"
                       "melpa"
                       "package"
                       "parenface"
                       "popup-git"
                       "powerline"
                       "pp-c-l"
                       ;; "pretty-symbols-mode"
                       "projectile"
                       "quickrun"
                       "s"
                       "shell-pop"
                       "smex"
                       ;; "smooth-scrolling"
                       "wgrep"
                       "whitespace"
                       "woman"
                       ;;"undohist"
                       "wgrep-ack"
                       )
           ;; set this to t if you want to manage this module yourself
           ;; instead of using the builtin package loading infrastructure
           :unmanaged-p nil)


;;;; Remove these 2 blocks if the module is unmanaged
(sm-module-pre (base)
  (ido-mode t)
  )

(sm-module-post (base)
  ;;; ------------------------------------- Scrolling -------------------------------------
  ;; (setq scroll-step 1)
  ;; (setq scroll-conservatively 1000)
  ;; (setq auto-window-vscroll nil)

  ;;; --------------------------------- Smotitah aliases ----------------------------------
  (defalias 'em 'sm-edit-module)
  (defalias 'ep 'sm-edit-package)
  (defalias 'epr 'sm-edit-profile)

  ;; insert titled comment
  (defvar *titled-comment-length* 90
    "Total width of comments inserted with `insert-titled-comment'")

  (defun length-of-region(start end)
    "Places the length of the current region in the kill ring."
    (interactive "r")
    (kill-new (message "%S" (- end start))))

  (defun insert-titled-comment (string)
    "Inserts a comment in the stile
;;;; --------------------------------- STRING expansion ----------------------------------
The number of dashes is calculated based on `*titled-comment-length*'.
"
    (interactive "PsTitle: ")
    (let* ((clen (length string))
           (comment-prefix (format "%s " (s-repeat 4 comment-start)))
           (remaining-space (- *titled-comment-length* (length comment-prefix) (+ 2 clen)))
           (dashes-left (s-repeat (floor (/ remaining-space 2.0)) "-"))
           (dashes-right (s-repeat (ceiling (/ remaining-space 2.0)) "-")))
      (insert (format "%s%s %s %s\n" comment-prefix dashes-left string dashes-right))))


  ;;; --------------------------------- before-save-hook ----------------------------------
  (add-hook 'before-save-hook (lambda () (unless (or (ignore-errors makefile-mode)
                                                     (memq major-mode '(ipa-mode
                                                                        makefile-mode)))
                                           (delete-trailing-whitespace))))


  ;;; ---------------------------------- Bindings ----------------------------------
  (define-key key-translation-map (kbd "C-.") (kbd "M-TAB"))
  (global-set-key [f7] 'magit-status)
  (global-set-key (kbd "C-\:") 'message-point)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))



  ;; Frame title
  (setq frame-title-format '(buffer-file-name "%b - emacs" ("%b - emacs")))

  ;;; AUTO-MODES
  (add-to-list 'auto-mode-alist (cons "\\.zsh$" 'shell-script-mode))

  ;;; Unique buffer names
  (require 'uniquify)
  (global-subword-mode 1)

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
  (defun open-notes-file ()
    (interactive)
    (find-file-other-window "~/.emacs.d/notes.org"))

  (global-set-key [(f9)] 'open-notes-file)

  )


(sm-provide :module base)
;;;; End base module
