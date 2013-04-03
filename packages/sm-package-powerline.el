;;;; Package powerline
(sm-package powerline
            :package-manager "package"
            :unmanaged-p nil)

(require 'powerline)

(defface powerline-enotify-bg-face
  '((((class color))
     (:background "gray20" :foreground "white" :weight bold))
    (t (:weight bold)))
  "face to fontify Enotify Success messages"
  :group 'powerline)

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

(add-hook 'post-command-hook (lambda ()
                               (setq powerline-selected-window (selected-window))))

(defun powerline-evil-face (active)
  (let ((face (intern (concat "powerline-evil-" (symbol-name evil-state) "-face"))))
    (cond ((and active (facep face))
           face)
          (active 'powerline-active2)
          (t 'powerline-inactive2))))

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
  (let ((rendered-str (format-mode-line str)))
    (powerline-merge-face-in--string
     (concat (when (and rendered-str (memq pad '(t l))) " ")
             (if (listp str) rendered-str str)
             (when (and rendered-str (memq pad '(t r))) " "))
     face)))


(defpowerline powerline-evil
  (replace-regexp-in-string "[<> ]" "" (eval (evil-state-property evil-state :tag))))

(defpowerline powerline-enotify
  (apply #'concat enotify-mode-line-string))


(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active (eq powerline-selected-window (selected-window)))
                        (face1 (if active 'powerline-active1 'powerline-inactive1))
                        (face2 (if active 'powerline-active2 'powerline-inactive2))
                        (pl-evil-face (powerline-evil-face active))
                        (lhs (list
                              ;; NORMAL> foobar.txt > + > Fundamental >                 [ ] < 5: 0 < all
                              ;; NORMAL>
                              (powerline-evil pl-evil-face 'l)
                              (powerline-raw " " pl-evil-face 'l)
                              (powerline-arrow-right pl-evil-face nil)
                              ;; foobar.txt
                              (powerline-raw "%z")
                              (powerline-raw (mode-line-eol-desc))
                              (powerline-buffer-id 'bold 'l)
                              (powerline-raw " ")
                              (powerline-arrow-right nil face2)
                              ;; > * >
                              (powerline-raw "%* " `((:weight bold :inherit ,face2)) 'l)
                              ;; (powerline-buffer-size face2 'l)
                              (powerline-arrow-right face2 face1)

                              ;; Fundamental .. >
                              (powerline-major-mode face1 'l)
                              (powerline-minor-modes face1 'l)
                              (powerline-raw " " face1)
                              (powerline-raw mode-line-process face1 'l)

                              (powerline-narrow face1 'l)

                              (powerline-arrow-right face1 nil)

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


                              (powerline-arrow-left nil face1)


                              (powerline-raw "%4l" face1 'r)
                              (powerline-raw ":" face1)
                              (powerline-raw "%3c" face1 'r)

                              (powerline-arrow-left face1 nil)
                              (powerline-raw " ")

                              (powerline-raw "%6p" nil 'r)
                              (powerline-raw-preserve enotify-mode-line-string
                                                      'powerline-enotify-bg-face 't)

                              (powerline-hud face2 face1))))
                   (concat
                    (powerline-render lhs)
                    (powerline-fill nil (powerline-width rhs))
                    (powerline-render rhs))))))

(sm-provide :package powerline)
