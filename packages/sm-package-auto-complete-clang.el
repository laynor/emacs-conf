;;;; Package auto-complete-clang
(sm-package auto-complete-clang
            :package-manager "package"
            :unmanaged-p nil)

(require 'auto-complete-clang)

;; Do not insert function signature in the buffer
(setq ac-source-clang
      (append '((document . nil))
              ac-source-clang))

(defadvice ac-clang-candidate (after ac-clang-candidate-show-signature-in-summary activate)
  (setq ad-return-value
        (mapcar (lambda (item) (propertize item
                                           'summary (ac-clang-document item)))
                ad-return-value)))

(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

;;; Kludge for objc
(defun ac-template-action ()
  (interactive)
  (message "sporchi")
  (unless (null ac-template-start-point)
    (message "??? %S" ac-last-completion)
    (let ((pos (point)) sl (snp "")
          (s (get-text-property 0 'raw-args (cdr ac-last-completion))))
      (cond ((string= s "")
             ;; function ptr call
             (setq s (cdr ac-last-completion))
             (setq s (replace-regexp-in-string "^(\\|)$" "" s))
             (setq sl (ac-clang-split-args s))
             (cond ((featurep 'yasnippet)
                    (dolist (arg sl)
                      (setq snp (concat snp ", ${" arg "}")))
                    (condition-case nil
                        (yas/expand-snippet (concat "("  (substring snp 2) ")")
                                            ac-template-start-point pos) ;; 0.6.1c
                      (error
                       ;; try this one:
                       (ignore-errors (yas/expand-snippet
                                       ac-template-start-point pos
                                       (concat "("  (substring snp 2) ")"))) ;; work in 0.5.7
                       )))
                   ((featurep 'snippet)
                    (delete-region ac-template-start-point pos)
                    (dolist (arg sl)
                      (setq snp (concat snp ", $${" arg "}")))
                    (snippet-insert (concat "("  (substring snp 2) ")")))
                   (t
                    (message "Dude! You are too out! Please install a yasnippet or a snippet script:)"))))
             (t
             (unless (string= s "()")
               (message "!!!! %S" s)
               (setq s (replace-regexp-in-string "{#" "" s))
               (setq s (replace-regexp-in-string "#}" "" s))
               ;;;KLUDGE -- objc argument list fix for yasnippet template
               (let ((p1 (search "<#" s))
                     (p2 (search "#>" s)))
                 (when (or (and (find ?: s) (> p1 p2))
                           (and (null p1) (null p2)))
                   (setq s (concat ":<#" s "#>]"))))
               ;;; KLUDGE end
               (cond ((featurep 'yasnippet)
                      (setq s (replace-regexp-in-string "<#" "${" s))
                      (setq s (replace-regexp-in-string "#>" "}" s))
                      (setq s (replace-regexp-in-string ", \\.\\.\\." "}, ${..." s))
                      (message ">>%S" s)
                      (condition-case nil
                          (yas/expand-snippet s ac-template-start-point pos) ;; 0.6.1c
                        (error
                         ;; try this one:
                         (ignore-errors (yas/expand-snippet ac-template-start-point pos s)) ;; work in 0.5.7
                         )))
                     ((featurep 'snippet)
                      (delete-region ac-template-start-point pos)
                      (setq s (replace-regexp-in-string "<#" "$${" s))
                      (setq s (replace-regexp-in-string "#>" "}" s))
                      (setq s (replace-regexp-in-string ", \\.\\.\\." "}, $${..." s))
                      (snippet-insert s))
                     (t
                      (message "Dude! You are too out! Please install a yasnippet or a snippet script:)")))))))))




(sm-provide :package auto-complete-clang)
