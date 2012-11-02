;;;; Package evil
(sm-package evil
            :package-manager "package"
            :unmanaged-p nil)

(require 'evil)
(evil-mode 1)
(evil-define-key 'motion Info-mode-map
  (kbd "<XF86Back>") 'Info-history-back
  (kbd "<XF86Forward>") 'Info-history-forward)

(define-key evil-insert-state-map [remap newline] 'evil-ret)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-S-k") 'evil-insert-digraph)
(define-key evil-insert-state-map (kbd "\C-e") nil)
(define-key evil-insert-state-map (kbd "\C-y") nil)
(evil-define-key 'normal emacs-lisp-mode-map (kbd "M-.") 'find-function)
(evil-define-key 'normal lisp-mode-map (kbd "M-.") 'slime-edit-definition)
(define-key evil-normal-state-map (kbd "M-.") nil)

(sm-provide :package evil)
