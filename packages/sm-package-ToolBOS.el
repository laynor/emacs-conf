;;;; Package ToolBOS
(sm-package ToolBOS
            :unmanaged-p t)

(defgroup hri
  nil
  "HRI stuff, mostly C related."
  :prefix 'hri-)

(defun hri-see-project-files ()
  (interactive)
  (find-dired (read-directory-name "Project Root") "-name '*.[hc]' -not -name '.*'")
  (dired-mark-files-regexp ".*"))

(c-add-style "hri"
	     '("awk"
	       (c-basic-offset . 2)
	       (c-cleanup-list paren-brace)
	       (c-hanging-braces-alist
		(substatement-open before after)
		(block-close before))
	       (c-offsets-alist
		(defun-block-intro . +)
		(statement-block-intro . +)
		(statement-cont . +)
		(case-label . +)
		(topmost-intro . 0)
		(topmost-intro-cont . 0))))
(setq c-default-style "hri")

(push '("\\packageVar$" . makefile-mode) auto-mode-alist)
(push '("\\makeRoot$" . makefile-mode) auto-mode-alist)
(push '("\\makeVar$" . makefile-mode) auto-mode-alist)
(push '("\\TcshSrc$" . shell-script-mode) auto-mode-alist)
(push '("\\BashSrc$" . shell-script-mode) auto-mode-alist)

(defcustom hri-keywords '(("\\<\\(ANY_LOG\\)" 1 font-lock-comment-face t)
                          ("\\<\\(ANY_REQUIRE\\)" 1 font-lock-comment-face t)
                          ("\\<\\(ANY_REQUIRE_MSG\\)" 1 font-lock-comment-face t))
  "Additional C keywords for HRI."
  :type '(repeat (cons string face))
  :group 'hri)

(defface hri-macros-face  '((t (:inherit font-lock-comment-face)))
  "Face used to highlight some HRI macros."
  :group 'hri)

(defun hri-add-keywords ()
  "adds a few special keywords for c and c++ modes"
  (font-lock-add-keywords nil
   '(
     ; Add keywords here
     ("\\<\\(ANY_LOG\\)\\>" . 'hri-macros-face )
     ("\\<\\(ANY_REQUIRE_MSG\\)\\>" . 'hri-macros-face )
     ("\\<\\(ANY_REQUIRE\\)\\>" . 'hri-macros-face )
     )))

(add-hook 'c-mode-hook 'hri-add-keywords)
(add-hook 'c-mode-hook (lambda () (c-toggle-auto-newline 1)))

(defun hri-last-nonwhitespace-char ()
  (save-excursion
    (when (ignore-errors (re-search-backward "[^[:space:]\n]"))
      (char-after (point)))))

(defun hri-f-def-or-call ()
  (save-excursion
    (when (char-equal (char-before (point)) ?\))
      (backward-list)
      (forward-char))
    (when (char-equal (hri-last-nonwhitespace-char) ?\()
      (when (ignore-errors (re-search-backward "[^[:space:]\n]"))
        (when (ignore-errors (re-search-backward "[^[:space:]\n]"))
          (= (car (syntax-after (point))) 2))))))

(defadvice c-electric-paren (after hri-c-paren-insert-space (arg) activate)
  (when (and (equal c-indentation-style "hri")
             (memq 'paren-brace c-cleanup-list)
             (hri-f-def-or-call))
    (cond ((char-equal last-command-event ?\()
           (just-one-space))
          ((char-equal last-command-event ?\))
           (save-excursion
             (backward-char)
             (just-one-space)
             (when (char-equal (char-before (1- (point))) ?\()
               (backward-char)
               (delete-char 1)))))))

(defcustom hri-toolbos-core-root (concat (getenv "HGR") "/DevelopmentTools/ToolBOSCore/2.0/")
  "The ToolBOSCore library root."
  :group 'hri
  :type 'directory)

(defun hri-toolbos-includes ()
  (concat toolbos-core-root "include/"))

(defcustom hri-completing-read-directory-function
  (if ido-mode
      'ido-read-directory-name
    read-directory-name)
  "The function used to read a directory name from the minibuffer."
  :group 'hri
  :type 'function)

(defcustom hri-completing-read-function
  (if ido-mode
      'ido-completing-read
    completing-read)
  "The function used for completing read."
  :group 'hri
  :type 'function)

(defcustom hri-package-types
  '("C/BBCM"
    "C/BBDM"
    "C/Library"
    "C/MainProgram"
    "Cpp/Class"
    "Cpp/MainProgram"
    "External/with/compilation"
    "External/without/compilation"
    "HDot/Application/HighLevel"
    "HDot/Application/LowLevel"
    "HDot/Application/Sampling"
    "HDot/Component/Approximation/Model"
    "HDot/Component/Evaluation/MultiObjective"
    "HDot/Component/Evaluation/SingleObjective"
    "HDot/Component/Optimizer"
    "HDot/Component/SamplingPlan"
    "master")
  "Package types supported by BST.py.
Replace the _ in the template directory names with a /."
  :group 'hri
  :type '(repeat string))

(defun hri-create-package (base-directory package-type package-name version)
  (interactive (list (if (eq major-mode 'dired-mode)
			 (dired-current-directory)
		       (funcall hri-completing-read-directory-function "Base Directory: " nil nil t nil))
		     (funcall hri-completing-read-function "Package type: " hri-package-types nil t)
		     (read-from-minibuffer "Package Name: ")
		     (read-from-minibuffer "Version: ")))
  (message "Calling with %S" (list base-directory package-type package-name version))
  (let ((path (concat base-directory "/" package-name "/" version)))
    (cond ((file-exists-p path)
	   (error "Cannot create an HRI package at '%s': the directory already exists."
		  path))
	  (t (let ((default-directory base-directory))
	       (with-temp-buffer
		 (call-process "BST.py" nil (current-buffer) t
			       "-n"
			       (replace-regexp-in-string "/" "_" package-type)
			       package-name
			       version)
		 (message (buffer-string))))
	     (when (eq major-mode 'dired-mode)
	       (revert-buffer))))))




(sm-provide :package ToolBOS)
