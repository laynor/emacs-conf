(defun beginning-and-end-of-sexp ()
  (destructuring-bind (b . e)
      (save-excursion
        (forward-char)
        (bounds-of-thing-at-point 'sexp))
    (cons b e)))

(evil-define-motion evil-forward-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let ((lookahead-1 (char-syntax (char-after (point))))
          (lookahead-2 (char-syntax (char-after (1+ (point)))))
          (new-point (point)))
      (condition-case nil
          (progn (save-excursion
                   (message "lookahead1 = %S, lookahead-2 = %S"
                            (string lookahead-1) (string lookahead-2))
                   (cond ((or (memq lookahead-2 '(?\ ?>))
                              (member lookahead-1 '(?\ ?>)))
                          (forward-char)
                          (skip-syntax-forward "->")
                          (setq new-point (point)))
                         (t (unless (memq lookahead-1 '(?\" ?\())
                              (forward-char))
                            (paredit-forward)
                            (backward-char)
                            (setq new-point (point)))))
                 (goto-char new-point))
        (error (error "End of sexp"))))))

(evil-define-motion evil-backward-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let ((lookahead (char-syntax (char-after (point))))
          (new-point (point)))
      (condition-case nil
          (progn (save-excursion
                   (when (memq lookahead '(?\) ?\"))
                     (forward-char))
                   (paredit-backward)
                   (setq new-point (point)))
                 (goto-char new-point))
        (error (error "Beginning of sexp"))))))

(evil-define-motion evil-enter-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let ((lookahead-1 (char-syntax (char-after (point))))
          (lookahead-2 (char-syntax (char-after (1+ (point)))))
          (lookbehind-1 (char-syntax (char-before (point))))
          (lookbehind-2 (char-syntax (char-before (1- (point))))))
      (cond ((and (= lookahead-1 ?\()
                  (/= lookbehind-1 ?\\)
                  (= (char-after (1+ (point))) ?\n))
             (forward-char)
             (skip-syntax-forward "-"))
            ((and (= lookahead-1 ?\()
                  (/= lookbehind-1 ?\\)
                  (/= lookahead-2 ?\)))
             ;; do not move the cursor if it's on the opening paren of ()
             (forward-char)
             (skip-syntax-forward "-"))
            ((and (= lookahead-1 ?\))
                  (or (/= lookbehind-1 ?\( )
                      (= lookbehind-2 ?\\)))
             ;; do not move the cursor if it's on the closing paren of ()
             (skip-syntax-backward "-")
             (backward-char))
            (t (error "Already at the deepest level"))))))

;; Does not work correctly when there are spaces after parens
;; does not work correctly when inside a string, check paredit.
;; check when there are spaces before parens
;; When the cursor is on an open paren, go up one level on an open paren
(require 'paredit)
(evil-define-motion evil-exit-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let (op-pos cl-pos)
      (condition-case nil
          (progn (save-excursion
                   (paredit-backward-up)
                   (setq op-pos (point))
                   (paredit-forward)
                   (setq cl-pos (point)))
                 (let ((lookahead (char-syntax (char-after (point)))))
                   (case lookahead
                     (?\( (goto-char op-pos))
                     (?\) (goto-char cl-pos))
                     (otherwise (goto-char (if (> (abs (- (point) cl-pos))
                                                  (abs (- (point) op-pos)))
                                               op-pos
                                             cl-pos))))))
        (error (error "Already at top-level."))))) )

(provide 'evil-sexp)
