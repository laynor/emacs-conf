(deftheme ale-black
  "Created 2012-06-18.")

(custom-theme-set-variables
 'ale-black
 )

(custom-theme-set-faces
 'ale-black
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :family "terminus"))))
 '(bold ((t (:weight bold))))
 '(bow ((t (:inherit nil :background "gray80" :foreground "black"))))
 '(cursor ((t (:background "#CC00CC"))))
 '(erc-keyword-face ((t (:foreground "magenta" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "medium slate blue"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "grey65"))))
 '(font-lock-comment-face ((t (:foreground "grey40"))))
 '(font-lock-constant-face ((t (:foreground "#ff0022"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "purple3"))))
 '(font-lock-fic-face ((((class color)) (:background "Red" :foreground "Yellow" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "deeppink" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "dodger blue" :weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "yellow"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground "dark slate grey"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground "steel blue"))))
 '(font-lock-type-face ((t (:foreground "spring green" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "forest green" :weight bold))))
 '(geiser-font-lock-autodoc-current-arg ((t (:background "dim gray" :weight bold))))
 '(hl-line ((t (:background "grey13"))))
 '(hri-macros-face ((t (:foreground "dark slate blue"))))
 '(ido-first-match ((t (:slant italic :weight bold))))
 '(ido-subdir ((t (:foreground "Dodgerblue"))))
 '(info-header-node ((t (:inherit info-node :foreground "gold"))))
 '(info-header-xref ((t (:inherit info-xref :foreground "yellow"))))
 '(info-menu-header ((t (:inherit variable-pitch :foreground "DodgerBlue1" :weight bold :height 1.5))))
 '(info-node ((t (:foreground "yellow" :slant italic :weight bold))))
 '(info-title-4 ((t (:inherit variable-pitch :foreground "dark violet" :weight bold))))
 '(info-xref-visited ((t (:inherit (link-visited info-xref)))))
 '(link ((t (:foreground "DodgerBlue3" :underline t))))
 '(link-visited ((t (:inherit link :foreground "purple3"))))
 '(menu ((t (:foreground "red" :height 80))))
 '(minibuffer-prompt ((t (:foreground "LimeGreen"))))
 '(mode-line ((t (:background "grey" :foreground "black" :box (:line-width 2 :color "grey" :style released-button) :height 1.0 :family "Segoe UI"))))
 '(mode-line-buffer-id ((t (:slant italic :weight bold :height 0.9 :family "Segoe UI"))))
 '(paren-face ((t (:foreground "dim gray"))))
 '(pp^L-highlight ((t (:foreground "#440044" :box (:line-width 1 :style pressed-button)))))
 '(show-paren-mismatch ((t (:background "white" :foreground "red"))))
 '(slime-repl-inputed-output-face ((t (:foreground "maroon1"))))
 '(trailing-whitespace ((t (:background "grey50"))))
 '(w3m-anchor ((t (:inherit link :foreground "red"))))
 '(w3m-arrived-anchor ((t (:inherit link :foreground "magenta"))))
 '(w3m-current-anchor ((t (:underline t :weight bold))))
 '(w3m-header-line-location-title ((t (:background "Gray15" :foreground "dodger blue"))))
 '(w3m-image ((t (:inherit black-on-white))))
 '(woman-bold ((t (:inherit bold :foreground "deep sky blue")))))

(provide-theme 'ale-black)
