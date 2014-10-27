;;;; Package slime
(sm-package slime
            :unmanaged-p t)

(load "~/quicklisp/slime-helper.el")
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-lisp-implementations
      '((sbcl ("sbcl"))
	(kawa ("/opt/java6/bin/java"
               "-Xss950k" ; compiler needs more stack
	       "-cp" "/home/ale/local/src/kawa/kawa-1.13.1.jar:/opt/java6/lib/tools.jar"
	       "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"
	       "kawa.repl" "-s" "--output-format" "readable-scheme")
              :init kawa-slime-init)
        (clojure ("clojure") :init swank-clojure-init)))

(defun kawa-slime-init (file _)
  (setq slime-protocol-version 'ignore)
  (let* ((compiled "/home/ale/local/src/slime/contrib/swank-kawa.zip")
	 (source "/home/ale/local/src/slime/contrib/swank-kawa.scm")
	 (swank (if (file-exists-p compiled)
		    `(load ,(expand-file-name compiled))
		  `(require ,(expand-file-name source)))))
    (format "%S\n"
	    `(begin ,swank (start-swank ,file)))))

(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-asdf))

(require 'slime)

(global-set-key [f6] 'slime)

;;; Show hyperspec in eww
(defadvice slime-hyperspec-lookup (around browse-with-eww activate)
  (flet ((browse-url (url) (eww-browse-url url)))
    ad-do-it))


(defadvice slime-repl-insert-prompt (after beginning-of-line-at-end-of-prompt () activate)
  (let ((inhibit-read-only t))
    (goto-char slime-repl-input-start-mark)
    (add-text-properties (line-beginning-position) (line-end-position)
                         '(read-only fence
                           inhibit-line-move-field-capture t
                           field output
                           rear-nonsticky t
                           front-sticky (field
                                         inhibit-line-move-field-capture)
                           fontified t))))

(sm-provide :package slime)
