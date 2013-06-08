;; (defun evil-sexp-enter ()
;;   (interactive)
;;   (cond ((and (= (char-after (point)) ?\()
;;               (= (char-after (1+ (point))) ?\n))
;;          (forward-char 2))
;;         ((= (char-after (point)) ?\()
;;          (forward-char))
;;         ((= (char-after (point)) ?\))
;;          (backward-char))
;;         ))

;; (define-key evil-motion-state-map (kbd "M-j") 'evil-sexp-enter)
;; (define-key evil-motion-state-map (kbd "M-k") 'backward-up-list)
;; (define-key evil-motion-state-map (kbd "M-h") '(lambda ()
;;                                                  (interactive)
;;                                                  (cond ((eq (char-after (point)) ?\))
;;                                                         (forward-char) (backward-sexp))
;;                                                        (t (backward-sexp)))))
;; (define-key evil-motion-state-map (kbd "M-l") (lambda ()
;;                                                 (interactive)
;;                                                 (unless (= (char-after (point)) ?\()
;;                                                   (forward-char))
;;                                                 (forward-sexp)
;;                                                 (backward-char)))
(evil-define-motion evil-forward-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let ((syntax-after-point (char-syntax (char-after (point))))
          (new-point (point)))
      (condition-case nil
          (progn (save-excursion
                   (unless (= syntax-after-point ?\()
                     (forward-char))
                   (forward-sexp)
                   (backward-char)
                   (setq new-point (point)))
                 (goto-char new-point))
        (error (error "End of sexp"))))))


(evil-define-motion evil-backward-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let ((syntax-after-point (char-syntax (char-after (point))))
          (new-point (point)))
      (condition-case nil
          (progn (save-excursion
                   (when (= syntax-after-point ?\))
                     (forward-char))
                   (backward-sexp)
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
;; check when there are spaces before parens
;; When the cursor is on an open paren, go up one level on an open paren
(evil-define-motion evil-exit-sexp (count)
  :type inclusive
  (dotimes (i (or count 1))
    (let (op-pos cl-pos)
      (condition-case nil
          (progn (save-excursion
                   (backward-up-list)
                   (setq op-pos (point))
                   (forward-sexp)
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
