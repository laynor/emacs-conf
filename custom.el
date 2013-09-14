(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.3)
 '(ac-clang-complete-executable
   "\"~/.emacs.d/site-lisp/emacs-clang-complete-async/clang-complete\"")
 '(ac-modes
   (quote
    (emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode python-mode ruby-mode lua-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode ts-mode sclang-mode verilog-mode erlang-mode)))
 '(ac-quick-help-delay 0.5)
 '(ac-use-fuzzy t)
 '(ac-use-menu-map t)
 '(blink-cursor-mode t)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function (quote browse-url-firefox))
 '(c-default-style
   (quote
    ((c-mode . "k&r")
     (c++-mode . "ellemtel")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(column-number-mode t)
 '(comment-style (quote extra-line))
 '(compilation-scroll-output (quote first-error))
 '(custom-enabled-themes (quote (ale-black-2)))
 '(custom-safe-themes
   (quote
    ("fb40d3205d941d8e54050cc7f70f9aca3046bde2ba2929d3f09f1ae02d248ae5" "91d2e0d23703423c1da4fad78cfa9717335d1a4ebbb1710fc214743822a5c89f" "490739aa40b07e854d5d816443a01577f19dd382a759187d258e283ea988eafb" "0a1af5e5832a8e38710ce55f3f04342f890330e586e42557c18638f7b056410f" "7f96e6a6dd9b02aeae63cde2916f2725eaf91b44049cdc9cf608ceff4c2a3732" "238da0c6bc2420cac5f2b20f714a01f0e042ea3bd48146ad9fc6be6baf33594f" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(dired-listing-switches "-alh")
 '(direx:closed-icon "▸ ")
 '(direx:open-icon "▾ ")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(el-get-generate-autoloads nil)
 '(elmo-imap4-default-stream-type (quote ssl))
 '(enotify-mode-line-prefix "")
 '(enotify-mode-line-suffix "")
 '(evil-default-cursor (quote (t "magenta")))
 '(evil-emacs-state-modes
   (quote
    (direx:direx-mode eassist-mode archive-mode bbdb-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ert-results-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-commit-mode magit-diff-mode magit-key-mode magit-log-mode magit-mode magit-reflog-mode magit-show-branches-mode magit-stash-mode magit-status-mode magit-wazzup-mode magit-key-mode mh-folder-mode monky-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode eclim-project-mode eclim-problems-mode wl-summary-mode wl-folder-mode quickrun/mode)))
 '(evil-motion-state-modes
   (quote
    (apropos-mode Buffer-menu-mode calendar-mode color-theme-mode command-history-mode compilation-mode dictionary-mode ert-results-mode help-mode Info-mode Man-mode speedbar-mode undo-tree-visualizer-mode view-mode woman-mode el-get-package-menu-mode)))
 '(flycheck-flake8rc "~/.config/flake8")
 '(fringe-mode (quote (4 . 4)) nil (fringe))
 '(git-commit-summary-maxlen 90)
 '(global-diff-hl-mode t)
 '(gtags-auto-update t)
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indentation turn-on-font-lock turn-on-eldoc-mode imenu-add-menubar-index)))
 '(ido-auto-merge-delay-time 1.0)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".*~$")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ipa-annotation-face (quote ipa-face))
 '(ipa-overlay-position "above")
 '(jedi:cut-summary-postfix "...")
 '(jedi:max-summary-length 40)
 '(jedi:show-function-signature-in-summary t)
 '(magit-diff-refine-hunk t)
 '(menu-bar-mode nil)
 '(org-export-odt-inline-image-extensions (quote ("png" "jpeg" "jpg" "gif" "svg")))
 '(powerline-height 16)
 '(projectile-ack-function (quote ag-regexp))
 '(projectile-tags-command "gtags")
 '(python-shell-interpreter "python2")
 '(recentf-max-menu-items 50)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/.recentf")
 '(rsense-rurema-home "~/Documents/ruby-refm-1.9.2-dynamic-20110629/")
 '(safe-local-variable-values
   (quote
    ((eval progn
           (setenv "MANPATH"
                   (mapconcat
                    (function identity)
                    (remove-duplicates
                     (split-string
                      (concat
                       (getenv "MANPATH")
                       ":" "/home/alessandro/local/opt/toolbos/man/")
                      ":" t)
                     :test
                     (function equal))
                    ":"))
           (add-project-directories "ToolBOSCore/2.0/include/" "ToolBOSCore/2.0/srcToolBOSCore/" "ToolBOSCore/2.0/srcLogCollector/" "/usr/lib/jvm/java-1.6.0-openjdk-1.6.0.0.x86_64/include/"))
     (eval progn
           (setenv "MANPATH"
                   (mapconcat
                    (function identity)
                    (remove-duplicates
                     (split-string
                      (concat
                       (getenv "MANPATH")
                       ":" "/home/alessandro/local/opt/toolbos/man/")
                      ":" t)
                     :test
                     (function equal))
                    ":"))
           (add-project-directories "ToolBOSCore/2.0/include/" "ToolBOSCore/2.0/srcToolBOSCore/" "ToolBOSCore/2.0/srcLogCollector/"))
     (eval progn
           (setenv "MANPATH"
                   (mapconcat
                    (function identity)
                    (remove-duplicates
                     (split-string
                      (concat
                       (getenv "MANPATH")
                       ":"
                       (dir-locals-directory)
                       "/home/alessandro/local/opt/toolbos/man/")
                      ":" t)
                     :test
                     (function equal))
                    ":"))
           (add-project-directories "ToolBOSCore/2.0/include/" "ToolBOSCore/2.0/srcToolBOSCore/" "ToolBOSCore/2.0/srcLogCollector/"))
     (eval progn
           (setenv "MANPATH"
                   (mapconcat
                    (function identity)
                    (remove-duplicates
                     (split-string
                      (concat
                       (getenv "MANPATH")
                       ":"
                       (dir-locals-directory)
                       "ToolBOSCore/2.0/doc/man/")
                      ":" t)
                     :test
                     (function equal))
                    ":"))
           (add-project-directories "ToolBOSCore/2.0/include/" "ToolBOSCore/2.0/srcToolBOSCore/" "ToolBOSCore/2.0/srcLogCollector/"))
     (eval progn
           (setenv "MANPATH"
                   (mapconcat
                    (function identity)
                    (remove-duplicates
                     (split-string
                      (concat
                       (getenv "MANPATH")
                       ":"
                       (dir-locals-directory)
                       "ToolBOSCore/2.0/doc/man/")
                      ":" t)
                     :test
                     (function equal))
                    ":"))
           (setenv "BST_CMAKE_OPTIONS" "-DCMAKE_BUILD_TYPE=Debug")
           (add-project-directories "/home/alessandro/src/ToolBOSCore/ToolBOSCore/2.0/include/" "/home/alessandro/src/ToolBOSCore/ToolBOSCore/2.0/srcToolBOSCore/" "/home/alessandro/src/ToolBOSCore/ToolBOSCore/2.0/srcLogCollector/"))
     (c-file-style "hri")
     (eval progn
           (setenv "MANPATH"
                   (mapconcat
                    (function identity)
                    (remove-duplicates
                     (split-string
                      (concat
                       (getenv "MANPATH")
                       ":"
                       (dir-locals-directory)
                       "ToolBOSCore/2.0/doc/man/")
                      ":" t)
                     :test
                     (function equal))
                    ":"))
           (setenv "BST_CMAKE_OPTIONS" "-DCMAKE_BUILD_TYPE=Debug")
           (add-project-directories "ToolBOSCore/2.0/include/" "ToolBOSCore/2.0/srcToolBOSCore/" "ToolBOSCore/2.0/srcLogCollector/"))
     (eval add-project-directories "ToolBOSCore/2.0/include/" "ToolBOSCore/2.0/srcToolBOSCore/" "ToolBOSCore/2.0/srcLogCollector/")
     (eval progn
           (add-project-directories
            (toolbos-includes)
            "1.0/src/")
           (c-set-style "hri"))
     (eval add-project-directories
           (toolbos-includes)
           "1.0/src/")
     (eval setq python-shell-virtualenv-path
           (file-truename "~/.virtualenvs/pyls"))
     (eval setq python-shell-virtualenv-path "/Users/alessandro/.virtualenvs/pyls")
     (python-shell-virtualenv-path expand-filename "~/.virtualenvs/pyls")
     (eval add-project-directories "/include/" "/third_party/dice-pjsip/pjmedia/include" "/third_party/dice-pjsip/pjlib/include" "/third_party/dice-pjsip/pjlib-util/include" "/third_party/dice-pjsip/pjsip/include" "/third_party/dice-pjsip/pjnath/include")
     (eval progn
           (message "stercoraro")
           (add-to-list
            (quote ac-clang-cflags)
            "-I/home/ale/src/C/prova"))
     (eval add-project-directories "include/" "third_party/dice-pjsip/pjmedia/include" "/third_party/dice-pjsip/pjlib/include" "/third_party/dice-pjsip/pjlib-util/include" "/third_party/dice-pjsip/pjsip/include" "/third_party/dice-pjsip/pjnath/include")
     (eval add-project-directories "include/")
     (eval progn
           (setq my-include-directories
                 (list
                  (concat
                   (file-name-directory
                    (file-truename load-file-name))
                   "include/")))
           (add-my-include-directories))
     (c-auto-newline)
     (c-file-style . k&r))))
 '(scroll-bar-mode nil)
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "<f2>")
 '(show-paren-mode t)
 '(starttls-extra-arguments (quote ("--insecure")))
 '(sublimity-scroll-weight1 8)
 '(sublimity-scroll-weight2 1.4)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\ *")
 '(uniquify-separator ":")
 '(visible-bell t)
 '(wl-summary-indent-length-limit nil)
 '(wl-summary-width nil)
 '(yas-prompt-functions
   (quote
    (yas-dropdown-prompt yas-completing-prompt yas-ido-prompt yas-no-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "microsoft" :family "Terminus"))))
 '(ac-candidate-face ((t (:background "lightgray" :foreground "black" :underline "light slate blue"))))
 '(ac-clang-candidate-face ((t (:background "lightgray" :foreground "navy" :underline "medium slate blue"))))
 '(diff-hl-change ((t (:background "slate blue" :foreground "blue3"))))
 '(diff-removed ((t (:inherit diff-changed :background "firebrick4"))))
 '(ediff-odd-diff-C ((t (:background "Dim Grey" :foreground "White"))) t)
 '(eldoc-highlight-function-argument ((t (:inherit bold :box nil :underline "deeppink"))))
 '(factor-font-lock-comment ((t (:inherit font-lock-comment-face))))
 '(factor-font-lock-parsing-word ((t (:inherit font-lock-keyword-face))))
 '(factor-font-lock-stack-effect ((t (:foreground "green"))))
 '(factor-font-lock-string ((t (:inherit font-lock-string-face))))
 '(factor-font-lock-word ((t (:inherit font-lock-function-name-face))))
 '(flx-highlight-face ((t (:inherit font-lock-keyword-face :underline t :weight bold))))
 '(flycheck-error ((t (:inherit error :underline t))))
 '(font-lock-comment-face ((t (:foreground "MediumPurple3" :slant italic))))
 '(font-lock-fic-face ((t (:inherit font-lock-comment-face :background "Red" :foreground "Yellow" :weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "#ff0033"))))
 '(font-lock-string-face ((t (:foreground "blue violet"))))
 '(font-lock-warning-face ((t (:inherit error :foreground "goldenrod"))))
 '(highlight-indentation-face ((t (:inherit fringe :background "grey8"))) t)
 '(hl-sexp-face ((t (:background "gray7"))))
 '(jedi:highlight-function-argument ((t (:inherit eldoc-highlight-function-argument))))
 '(magit-log-author ((t (:foreground "MediumPurple1"))))
 '(magit-log-sha1 ((t (:foreground "cornflower blue"))))
 '(mode-line ((t (:background "grey" :foreground "black" :height 100 :family "Consolas"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey30" :foreground "grey80" :weight light))))
 '(popup-scroll-bar-foreground-face ((t (:background "blue"))))
 '(powerline-enotify-bg-face ((t (:background "gray8" :foreground "grey75" :box (:line-width 1 :color "grey75" :style released-button) :slant italic :weight bold))))
 '(pp^L-highlight ((t (:background "dim gray" :foreground "purple4"))))
 '(region ((t (:background "#382D7B"))))
 '(ruby-dev-repl-prompt-face ((t (:inherit default :foreground "SpringGreen1")))))
