;;;; Package c-eldoc
(sm-package c-eldoc
            :package-manager "package"
            :unmanaged-p nil)

(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(defun c-eldoc-format-arguments-string (arguments index)
  "Formats the argument list of a function."
  (let ((paren-pos (string-match "(" arguments))
        (pos 0))
    (when paren-pos
      (setq arguments (replace-regexp-in-string "\\\\?[[:space:]\\\n]"
                                                " "
                                                (substring arguments paren-pos))
            arguments (replace-regexp-in-string "\\s-+" " " arguments)
            arguments (replace-regexp-in-string " *, *" ", " arguments)
            arguments (replace-regexp-in-string "( +" "(" arguments)
            arguments (replace-regexp-in-string " +)" ")" arguments))
      ;; find the correct argument to highlight, taking `...'
      ;; arguments into account
      (while (and (> index 1)
                  pos
                  (not (string= (substring arguments (+ pos 2) (+ pos 6))
                                "...)")))
        (setq pos (string-match "," arguments (1+ pos))
              index (1- index)))
      ;; embolden the current argument
      (when (and pos
                 (setq pos (string-match "[^ ,()]" arguments pos)))
        (add-text-properties pos (string-match "[,)]" arguments pos)
                             '(face eldoc-highlight-function-argument) arguments))
      arguments)))

(sm-provide :package c-eldoc)
