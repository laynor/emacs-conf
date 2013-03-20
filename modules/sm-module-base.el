;;;; This module should contain your basic configuration, that will be
;;;; shared by all the profiles - if they include the base module, of
;;;; course!

(sm-module "base"
           ;; add the packages required by your basic configuration here
  :require-packages '( "auto-complete"
                       "browse-kill-ring"
                       "diff-hl"
                       "dired"
                       "direx"
                       "erc"
                       "undo-tree"
                       "evil"
                       "fuzzy"
                       "gist"
                       "gitignore-mode"
                       "guide-key"
                       "hide-comnt"
                       "ido-ubiquitous"
                       "magit"
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
                       "s"
                       "smex"
                       ;; "smooth-scrolling"
                       "surround"
                       "wgrep"
                       "whitespace")
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
  (add-hook 'before-save-hook (lambda () (unless (ignore-errors makefile-mode) (delete-trailing-whitespace))))

  ;;; ---------------------------------- Bindings ----------------------------------
  (define-key key-translation-map (kbd "C-.") (kbd "M-TAB"))
  (global-set-key [f7] 'magit-status)
  (global-set-key (kbd "C-\;") 'message-point)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

  ;; Plug browse-kill-ring into evil
  (defadvice evil-paste-pop (around evil-kill-ring-browse-maybe (arg) activate)
    "If last action was not a yank, run `browse-kill-ring' instead."
    ;; yank-pop has an (interactive "*p") form which does not allow
    ;; it to run in a read-only buffer.  We want browse-kill-ring to
    ;; be allowed to run in a read only buffer, so we change the
    ;; interactive form here.  In that case, we need to
    ;; barf-if-buffer-read-only if we're going to call yank-pop with
    ;; ad-do-it
    (interactive "p")
    (if (not (memq last-command '(evil-paste-after evil-paste-before yank)))
        (browse-kill-ring)
      (barf-if-buffer-read-only)
      ad-do-it))

  ;; Auto complete and evil
  (define-key ac-completing-map (kbd "C-[") '(lambda () (interactive) (ac-stop) (evil-normal-state)))

  ;; Frame title
  (setq frame-title-format '(buffer-file-name "%b - emacs" ("%b - emacs")))

  ;;; AUTO-MODES
  (add-to-list 'auto-mode-alist (cons "\\.zsh$" 'shell-script-mode))

  ;;; Unique buffer names
  (require 'uniquify)

  ;; direx bindings

  (evil-global-set-key 'normal (kbd "C-d") 'direx:find-directory-other-window)
  )

(sm-provide :module base)
;;;; End base module
