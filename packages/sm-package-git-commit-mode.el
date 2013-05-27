;;;; Package git-commit-mode
(sm-package git-commit-mode
            :package-manager "package"
            :unmanaged-p nil)

(require 'git-commit-mode)

(makunbound 'git-commit-summary-regexp)

(defcustom git-commit-summary-maxlen 50
  "Summary length threshold for git summary"
  :group 'git-commit
  :type 'integer)

(defvar git-commit-summary-regexp
   (format "\\(?:^\\(.\\{,%d\\}\\)\\(.*?\\)$\\)"
           git-commit-summary-maxlen)
   "Regexp to match the summary line.

Do not use this expression directly, instead call
`git-commit-find-summary-regexp' to create a regular expression
to match the summary line.")

(defun git-commit-summary-maxlen-update ()
  (setq git-commit-summary-regexp
        (format "\\(?:^\\(.\\{,%d\\}\\)\\(.*?\\)$\\)"
                git-commit-summary-maxlen)))

(add-hook 'magit-log-edit-mode-hook #'git-commit-summary-maxlen-update)

(sm-provide :package git-commit-mode)
