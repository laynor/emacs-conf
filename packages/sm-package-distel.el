;;;; Package distel
(sm-package distel
            :package-manager nil
            :unmanaged-p t)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp/distel/elisp"))
(require 'distel)
(distel-setup)
;; TODO: hack erl-print-arglist to color the help text in minibuffer

(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

(sm-provide :package distel)
