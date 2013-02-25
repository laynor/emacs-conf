;;;; Package powerline
(sm-package powerline
            :package-manager "package"
            :unmanaged-p nil)

(require 'powerline)

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
          (t powerline-inactive2))))


(defpowerline powerline-evil
  (replace-regexp-in-string "[<> ]" "" (eval (evil-state-property evil-state :tag))))

(defpowerline powerline-enotify
  (apply #'concat enotify-mode-line-string))


(setq-default mode-line-format
              '("%e"
                (:eval
                 (let* ((active (eq (frame-selected-window) (selected-window)))
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
                              (powerline-raw mode-line-process face1 'l)

                              (powerline-narrow face1 'l)

                              (powerline-arrow-right face1 nil)

                              (powerline-vc nil)
                              ))
                        (rhs (list
                              ;; (powerline-raw global-mode-string nil 'r)
                              ;;(powerline-raw global-mode-string)
                              (apply #'concat enotify-mode-line-string)
                              (powerline-raw " ")
                              (powerline-raw (remove 'enotify-mode-line-string global-mode-string))

                              (powerline-arrow-left nil face1)

                              (powerline-raw "%4l" face1 'r)
                              (powerline-raw ":" face1)
                              (powerline-raw "%3c" face1 'r)

                              (powerline-arrow-left face1 nil)
                              (powerline-raw " ")

                              (powerline-raw "%6p" nil 'r)

                              (powerline-hud face2 face1))))
                   (concat
                    (powerline-render lhs)
                    (powerline-fill nil (powerline-width rhs))
                    (powerline-render rhs))))))

(sm-provide :package powerline)
