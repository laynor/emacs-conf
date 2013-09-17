;;;; Package pp-c-l
(sm-package pp-c-l
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(require 'pp-c-l)

(sm-integrate-with s
  (defvar my-pp-c-l-pattern "▄▀")
  (setq my-pp-c-l-pattern "\\\\•//•")
  (defun my-pp-c-l-string-function (win)
    (let* ((bname (buffer-name (window-buffer win)))
           (len (- (window-width win) (length bname) 2))
           (n (/ len (length my-pp-c-l-pattern))))
      (concat (s-repeat (floor (/ n 2.0)) my-pp-c-l-pattern)
              " " bname " "
              (s-repeat (ceiling (/ n 2.0)) my-pp-c-l-pattern))))

(setq pp^L-^L-string-function 'my-pp-c-l-string-function))

(pretty-control-l-mode t)

(sm-provide :package pp-c-l)
