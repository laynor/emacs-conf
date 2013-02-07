;;;; This module should contain your basic configuration, that will be
;;;; shared by all the profiles - if they include the base module, of
;;;; course!

(sm-module "base"
           ;; add the packages required by your basic configuration here
           :require-packages '("melpa" "evil" "magit" "smex" "ido-ubiquitous"
                               "gitignore-mode" "parenface" "s" "wgrep"
                               "pp-c-l" "erc" "dired" "browse-kill-ring"
                               "popup-git" "whitespace" "gist"
                               "markdown-mode" "markdown-mode+"
                               "auto-complete")
           ;; set this to t if you want to manage this module yourself
           ;; instead of using the builtin package loading infrastructure
           :unmanaged-p nil)


;;;; Remove these 2 blocks if the module is unmanaged
(sm-module-pre (base)
  (ido-mode t)
  )

(sm-module-post (base)
  (require 'uniquify)
  ;; smotitah aliases
  (defalias 'em 'sm-edit-module)
  (defalias 'ep 'sm-edit-package)
  (defalias 'epr 'sm-edit-profile)
  ;; insert titled comment
  (defvar *titled-comment-length* 90
    "Total width of comments inserted with `insert-titled-comment'")

  (defun package-depends (package)
    "Retrieves the packages PACKAGE depends on."
    (mapcar 'car (elt (cdr (assoc package package-alist)) 1)))

  (defun package-reverse-depends (package)
    "Returns a list of the currently installed packages that
depend on PACKAGE."
    (let* ((package-list (mapcar 'car package-alist))
           (deps (mapcar (lambda (p) (cons package (package-depends p)))
                         package-list)))
      (mapcar 'car (remove-if-not (lambda (dep) (memq package (cdr dep))) deps))))

  (defun insert-titled-comment (string)
    "Inserts a comment in the stile
;;;; --------------------------------- STRING expansion ----------------------------------
The number of dashes is calculated based on `*titled-comment-length*'.
"
    (interactive "sTitle: ")
    (let* ((clen (length string))
           (comment-prefix (format "%s " (s-repeat 4 comment-start)))
           (remaining-space (- *titled-comment-length* (length comment-prefix) (+ 2 clen)))
           (dashes-left (s-repeat (floor (/ remaining-space 2.0)) "-"))
           (dashes-right (s-repeat (ceiling (/ remaining-space 2.0)) "-")))
      (insert (format "%s%s %s %s\n" comment-prefix dashes-left string dashes-right))))


  ;; Before save hook
  (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

  ;;; global bindings
  (define-key key-translation-map (kbd "C-.") (kbd "M-TAB"))
  (global-set-key [f7] 'magit-status)
  (global-set-key (kbd "C-\;") 'message-point)
  ;; (defadvice magit-key-mode (after evil-magit-key-mode-in-emacs-state (for-group &optional original-opts) activate)
  ;;   (evil-emacs-state))
  (add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

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
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map (kbd "C-[") '(lambda () (interactive) (ac-stop) (evil-normal-state)))

  ;; Frame title
  (setq frame-title-format '(buffer-file-name "%b - emacs" ("%b - emacs")))

  ;;; AUTO-MODES
  (add-to-list 'auto-mode-alist (cons "\\.zsh$" 'shell-script-mode))
  )

(sm-provide :module base)
;;;; End base module
