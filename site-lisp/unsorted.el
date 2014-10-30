;; Eshell function


(defun set-file-coding (filename coding)
  (let ((do-not-kill (some (lambda (buf) (equal (buffer-file-name buf)
                                                filename))
                           (buffer-list))))
    (with-current-buffer (find-file-noselect filename)
      ((set-buffer-file-coding-system 'utf-8-unix)
       (save-buffer)
       (unless do-not-kill
         (kill-buffer))))))

;; light-table like docs

(defface ltdoc:documentation-face
    '((((class color))
       (:background "dodgerblue2" :foreground "white" :weight bold))
      (t (:weight bold)))
  "face to fontify Enotify Success messages"
  :group 'powerline)

(defvar ltdoc:current-symbol nil
  "The last symbol for which documentation has been shown.")
(defvar ltdoc:current-overlay nil
  "The overlay used for documentation in the current buffer")

(defvar ltdoc:find-symbol-for-documentation-function 'ltdoc:symbol-at-point-or-fnsym-in-current-sexp)

(make-variable-buffer-local 'ltdoc:current-overlay)

(defun enclosing-sexp-bounds ()
  (let (b)
    (save-excursion
      (paredit-backward-up)
      (setq b (point))
      (paredit-forward)
      (cons b (point)))))

(defun ltdoc:symbol-at-point-or-fnsym-in-current-sexp ()
  "Returns the symbol at point or the function symbol in the
enclosing sexp."
  (let* ((sap-1 (thing-at-point 'symbol))
         (sap (and sap-1 (intern sap-1)))
         (fnsym (car (elisp--fnsym-in-current-sexp))))
    (cond ((null fnsym)
	   nil)
	  ((and sap (not (eq sap fnsym)))
           (cons 'var sap))
          (t (cons 'fun fnsym)))))


(defun ltdoc::get-doc-header (symdescr)
  (when symdescr
    (cl-destructuring-bind (type . sym)
        symdescr
      (ignore-errors (substring-no-properties
                      (case type
                        (fun (or (elisp--get-fnsym-args-string sym)
                                 (elisp--get-var-docstring sym)))
                        (var (or (elisp--get-var-docstring sym)
                                 (elisp--get-fnsym-args-string sym)))))))))

(defun ltdoc::format-docs (symdescr)
  (let ((doc-header (ltdoc::get-doc-header symdescr)))
    (when doc-header
      (format "%s\n%s\n"
              (propertize doc-header
                          'face 'underline)
              (case (car symdescr)
                (var (or (documentation-property (cdr symdescr) 'variable-documentation)
                         (documentation (cdr symdescr))))
                (fun (or (documentation (cdr symdescr))
                         (documentation-property (cdr symdescr) 'variable-documentation))))))))


(defun ltdoc::show-docs (symdescr)
  (interactive)
  (let* ((eol (line-end-position))
         (docs (and symdescr (ltdoc::format-docs symdescr))))
    (cond (docs
           (setq ltdoc:current-symbol symdescr)
           (setq ltdoc:current-overlay (make-overlay eol (1+ eol)))
           (overlay-put ltdoc:current-overlay 'display docs)
           (overlay-put ltdoc:current-overlay 'face 'ltdoc:documentation-face)
           (overlay-put ltdoc:current-overlay 'before-string "\n")
           (overlay-put ltdoc:current-overlay 'after-string "\n"))
          (symdescr (message "No docs available for %s" (cdr symdescr))))))

(defun ltdoc:hide-docs ()
  (when ltdoc:current-overlay
    (delete-overlay ltdoc:current-overlay)
    (setq ltdoc:current-overlay nil)))

(defun ltdoc:dwim()
  (interactive)
  (let ((symdescr (funcall ltdoc:find-symbol-for-documentation-function)))
    (if (and ltdoc:current-overlay (equal symdescr ltdoc:current-symbol))
        (ltdoc:hide-docs)
      (ltdoc:hide-docs)
      (ltdoc::show-docs symdescr))))
