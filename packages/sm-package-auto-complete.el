;;;; Package auto-complete
(sm-package auto-complete
            :package-manager "package"
            :unmanaged-p nil)

(require 'auto-complete-config)

(ac-config-default)

(setq ac-completing-map
      (let ((map (make-sparse-keymap)))
	(define-key map "\t" 'ac-expand)
	(define-key map "\r" 'ac-complete)
	;; (define-key map [M-tab] 'ac-complete)
	(define-key map "\C-s" 'ac-isearch)

	;;(define-key map [M-n] 'ac-next)
	(define-key map [(control ?n)] 'ac-next)
	(define-key map [(control ?p)] 'ac-previous)
	(define-key map [down] 'ac-next)
	(define-key map [up] 'ac-previous)

	(define-key map [f1] 'ac-help)
	;; (define-key map [M-f1] 'ac-persist-help)
	(define-key map (kbd "C-/") 'ac-help)
	;; (define-key map [C-M-?] 'ac-persist-help)

	(define-key map [C-down] 'ac-quick-help-scroll-down)
	(define-key map [C-up] 'ac-quick-help-scroll-up)
	;; (define-key map [C-M-n] 'ac-quick-help-scroll-down)
	;; (define-key map [C-M-p] 'ac-quick-help-scroll-up)

	(dotimes (i 9)
	  (let ((symbol (intern (format "ac-complete-%d" (1+ i)))))
	    (fset symbol
		  `(lambda ()
		     (interactive)
		     (when (and (ac-menu-live-p) (popup-select ac-menu ,i))
		       (ac-complete))))
	    (define-key map (car (read-from-string (format "[C-%s]" (1+ i)))) symbol)))

	map))

(set-face-underline 'ac-candidate-face "grey")

(sm-provide :package auto-complete)
