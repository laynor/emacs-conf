;;;; Package powerline
(sm-package powerline
            :package-manager "package"
            :unmanaged-p nil)

(require 'powerline)

;; (defun pl/wrap-defun (name dir width let-vars body)
;;   "Return powerline function list of NAME in DIR with WIDTH using LET-VARS and BODY."
;;   (let* ((src-face (if (eq dir 'left) 'face1 'face2))
;;          (dst-face (if (eq dir 'left) 'face2 'face1)))
;;     `(defun ,(intern (format "powerline-%s-%s" name (symbol-name dir)))
;;        (face1 face2 &optional height)
;;        (when window-system
;;          (unless height (setq height (pl/separator-height)))
;;          (let* ,(append
;;                  `((color1 (when ,src-face
;;                              (pl/hex-color (face-attribute ,src-face :background))))
;;                    (color2 (when ,dst-face
;;                              (pl/hex-color (face-attribute ,dst-face :background))))
;;                    (colori (when (and color1 color2) (pl/interpolate color1 color2)))
;;                    (color1 (or color1 "None"))
;;                    (color2 (or color2 "None"))
;;                    (colori (or colori "None")))
;;                  let-vars)
;;            (create-image
;;             ,(append
;;               `(concat
;;                 (format "/* XPM */
;; static char * %s_%s[] = {
;; \"%s %s 3 1\",
;; \"0 c %s\",
;; \"1 c %s\",
;; \"2 c %s\",
;; "
;;                         ,(replace-regexp-in-string "-" "_" name) (symbol-name ',dir)
;;                         ,width height
;;                         color1
;;                         color2
;;                         colori))
;;               body
;;               '("};"))
;;             'xpm t :ascent 'center
;;             :face (when (and face1 face2) ,dst-face)))))))

;;; powerline-raw-preserve

(defun powerline-string-face-change-positions (string)
  (let ((x 0)
        p)
    (while x
      (push x p)
      (setq x (next-single-property-change x 'face string)))
    (push (length string) p)
    (cdr (nreverse p))))

(defun powerline-string-intervals-by-face (string)
  (let ((p (powerline-string-face-change-positions string))
        (x 0)
        l)
    (dolist (i p (reverse l))
      (push (substring string x i) l)
      (setq x i))))

(defun powerline-merge-faces (f1 f2)
  (if (or f1 f2)
      `((:inherit ,(remove nil (list f1 f2))))
    nil))



(defun powerline-merge-face-in-string-1 (string face)
  (propertize string 'face (powerline-merge-faces (get-text-property 0 'face string) face)))


(defun powerline-merge-face-in--string (string face)
  (reduce 'concat (mapcar (lambda (str)
                            (powerline-merge-face-in-string-1 str face))
                          (powerline-string-intervals-by-face string))))

(defun powerline-raw-preserve (str &optional face pad)
  "Like powerline-raw, but preserves the face attributes in STR
not defined in FACE.
For example, if face only specifies a background attribute, the
and STR has been propertized with a face that specifies a
foreground attribute, the string will be displayed with the
foreground specified by STR and the background specified by
FACE. This includes the mouse over faces."
  (let ((rendered-str (format-mode-line str)))
    (powerline-merge-face-in--string
     (concat (when (and rendered-str (memq pad '(t l))) " ")
             (if (listp str) rendered-str str)
             (when (and rendered-str (memq pad '(t r))) " "))
     face)))

;;; Evil integration
(sm-integrate-with (:package evil)

  (defface powerline-evil-insert-face
    '((((class color))
       (:background "green" :foreground "black" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defface powerline-evil-normal-face
    '((((class color))
       (:background "red" :foreground "black" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defface powerline-evil-visual-face
    '((((class color))
       (:background "yellow" :foreground "black" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defface powerline-evil-motion-face
    '((((class color))
       (:background "blue" :foreground "black" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defface powerline-evil-emacs-face
    '((((class color))
       (:background "blue violet" :foreground "black" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defface powerline-modified-flag-face
    '((((class color))
       (:weight bold :inherit 'powerline-active2))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline)

  (defun powerline-evil-face (active)
    (let ((face (intern (concat "powerline-evil-" (symbol-name evil-state) "-face"))))
      (cond ((and active (facep face))
             face)
            (active 'powerline-active2)
            (t 'powerline-inactive2))))

  (defpowerline powerline-evil
    (concat (replace-regexp-in-string "[<> ]" "" (eval (evil-state-property evil-state :tag))) " "))
  )


(sm-integrate-with (:package enotify)
  (defface powerline-enotify-bg-face
    '((((class color))
       (:background "gray20" :foreground "white" :weight bold))
      (t (:weight bold)))
    "face to fontify Enotify Success messages"
    :group 'powerline))



;;; active modeline detection hack
(add-hook 'post-command-hook (lambda ()
                               (when (not (minibuffer-selected-window))
                                 (setq powerline-selected-window (selected-window)))))

(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active (eq powerline-selected-window (selected-window)))
                        (face1 (if active 'powerline-active1 'powerline-inactive1))
                        (face2 (if active 'powerline-active2 'powerline-inactive2))
                        (mode-line (if active 'mode-line 'mode-line-inactive))
                        (pl-evil-face (ignore-errors (powerline-evil-face active)))
                        (separator-left
                         (intern (format "powerline-%s-%s"
                                         powerline-default-separator
                                         (car powerline-default-separator-dir))))
                        (separator-right
                         (intern (format "powerline-%s-%s"
                                         powerline-default-separator
                                         (cdr powerline-default-separator-dir))))
                        (powerline-evil (ignore-errors (powerline-evil pl-evil-face 'l)))
                        (lhs
                         (list
                          ;; NORMAL> foobar.txt > + > Fundamental >                 [ ] < 5: 0 < all
                          ;; NORMAL>

                          powerline-evil
                          (and powerline-evil (funcall separator-left pl-evil-face mode-line))
                          ;; foobar.txt
                          (powerline-raw "%z")
                          (powerline-raw (mode-line-eol-desc))
                          (powerline-buffer-id 'bold 'l)
                          (powerline-raw " ")
                          (funcall separator-left mode-line face2)
                          ;; > * >
                          (powerline-raw "%* " `((:weight bold :inherit ,face2)) 'l)
                          ;; (powerline-buffer-size face2 'l)
                          (funcall separator-left face2 face1)

                          ;; Fundamental .. >
                          (powerline-major-mode face1 'l)
                          (powerline-minor-modes face1 'l)
                          (powerline-raw " " face1)
                          (powerline-raw mode-line-process face1 'l)

                          (powerline-narrow face1 'l)

                          (funcall separator-left face1 mode-line)

                          (powerline-vc nil)
                          (powerline-raw-preserve erc-modified-channels-object)
                          ;; (powerline-raw (if (file-exists-p (scratch-palette-file)) "S" "") nil 'l)
                          ))
                        (rhs (list
                              ;; (powerline-raw global-mode-string nil 'r)
                              ;;(powerline-raw-preserve global-mode-string face1)
                              ;;(apply #'concat enotify-mode-line-string)
                              ;;(powerline-raw " ")
                              (powerline-raw-preserve (remove 'enotify-mode-line-string
                                                              global-mode-string)
                                                      nil 'r)


                              (powerline-arrow-right mode-line face1)


                              (powerline-raw "%4l" face1 'r)
                              (powerline-raw ":" face1)
                              (powerline-raw "%3c" face1 'r)

                              (powerline-arrow-left face1 nil)
                              (powerline-raw " ")

                              (powerline-raw "%6p" nil 'r)
                              (ignore-errors (powerline-raw-preserve enotify-mode-line-string
                                                                     'powerline-enotify-bg-face 't))

                              (powerline-hud face2 face1))))
                   (concat
                    (powerline-render lhs)
                    (powerline-fill nil (powerline-width rhs))
                    (powerline-render rhs))))))

(sm-provide :package powerline)
