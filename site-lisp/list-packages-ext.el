;;; list-package-extras.el --- Extras for list-packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: convenience, tools
;; Package-Requires: ((s "1.6.0"))
;; Version 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; TODO: undo support
;;; TODO: region support

;;

;;; Code:

;;;;  Macros

(require 'cl)
(require 's)
(require 'ht)
(require 'persistent-soft)

(defmacro* lpe::equal-case (exp &body clauses)
  "Like case, but comparison done with equal.
  (lpe::equal-case exp
     (p1 body1..)
     (p2 body2..)
     (_ body3))

expands to
  (let ((exp* exp))
    (cond ((equal exp* p1)
           body1...)
          ((equal exp* p2)
           body2...)
          (t body3...)))
"
  (let (body
        (exp* (gensym)))
    (dolist (clause clauses)
      (let* ((pattern (car clause))
             (condition (if (equal pattern '_)
                            t
                          `(equal ,exp* ,pattern))))
      (push `(,condition
              ,@(cdr clause))
            body)))
    `(let ((,exp* ,exp))
       (cond
        ,@(reverse body)))))

(put 'lpe::equal-case 'lisp-indent-function 1)



;;;;  Variables


(defvar list-packages-ext-mode-map (make-sparse-keymap))
(defvar lpe::*overlays* nil)
(defvar lpe::*filterfn* nil)
(defvar lpe::*show-hidden-p* nil)
(defvar lpe::*current-filter* "")
(defvar lpe::search-in-summary nil)
(defvar lpe::*tag->packages* (ht-create))
(defvar lpe::*package->tags* (ht-create))
(defvar lpe::*last-applied-tags* nil)



;;;;  Persistence

(defvar lpe::*cache-location* "list-packages-ext-mode")

(defun lpe::save-state ()
  (persistent-soft-store 'lpe::*tag->packages*
                         (ht-to-alist lpe::*tag->packages*)
                         lpe::*cache-location*)
  (persistent-soft-store 'lpe::*package->tags*
                         (ht-to-alist lpe::*package->tags*)
                         lpe::*cache-location*))

(defun lpe::resume-state ()
  (let ((t->p (persistent-soft-fetch 'lpe::*tag->packages* lpe::*cache-location*))
        (p->t (persistent-soft-fetch 'lpe::*package->tags* lpe::*cache-location*)))
    (setq lpe::*tag->packages* (or (and t->p (ht-from-alist t->p)) (ht-create)))
    (setq lpe::*package->tags* (or (and p->t (ht-from-alist p->t)) (ht-create)))))


;;;;  Minor mode


(define-minor-mode list-packages-ext-mode
  "Activate extras for list-packages"
  :global nil :group 'list-packages-ext
  :map list-packages-ext-mode-map
  :lighter "LPE"
  ;; TODO
  (cond (list-packages-ext-mode
         (lpe:activate))
        (t (lpe:deactivate))))


(defun lpe:activate ()
  (apply 'run-hooks list-packages-ext-mode-hook)
  (add-hook 'post-command-hook 'lpe::post-command-hook)
  (lpe::resume-state)
  (lpe::update-all))


(defun lpe:deactivate ()
  (remove-hook 'post-command-hook 'lpe::post-command-hook))


(defun lpe::update-all ()
  (lpe::show-all-lines)
  (lpe::process-buffer)
  (tabulated-list-print t)
  (lpe::update-minibuffer-info))


;;;;  Tags


(defun lpe::clear-tags ()
  (setq lpe::*tag->packages* (make-hash-table :test 'equal))
  (setq lpe::*package->tags* (make-hash-table :test 'equal)))


(defmacro lpe::tag->packages (tag)
  `(gethash ,tag lpe::*tag->packages*))


(defmacro lpe::package->tags (package)
  `(gethash ,package lpe::*package->tags*))


(defun lpe::package-desc-at-point ()
  (tabulated-list-get-id))


(defun lpe::package-at-point ()
  (let ((pd (lpe::package-desc-at-point)))
    (and pd (package-desc-name pd))))

(defun lpe::packages-in-region (beg end)
  (let (packages
        (beg (min beg end))
        (end (max beg end)))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (goto-char (line-beginning-position))
        (push (lpe::package-at-point) packages)
        (next-line)))
    packages))

(defun lpe::tags-at-point ()
  (lpe::package->tags (lpe::package-at-point)))


(defun lpe::tag-package (tag package)
  (let* ((packages-with-tag (gethash tag lpe::*tag->packages*))
         (tags-of-package (gethash package lpe::*package->tags*)))

    (cond ((s-starts-with? "!" tag)
           (let ((tag (subseq tag 1)))
             (message "Removing tag %s" tag)
             (setf tags-of-package (remove* tag tags-of-package :test 'equal))
             (setf packages-with-tag (remove* package packages-with-tag :test 'equal))))

          (t (pushnew tag tags-of-package :test 'equal)
             (pushnew package packages-with-tag :test 'equal)))
    (puthash tag packages-with-tag lpe::*tag->packages*)
    (puthash package tags-of-package lpe::*package->tags*)))


(defun lpe::all-tags ()
  (let (all-tags)
    (union (list "hidden")
               (ht-keys lpe::*tag->packages*) :test 'equal)))

(defun* lpe::tag% (taglist packages add)
  (setf lpe::*last-applied-tags* taglist)
  (dolist (pkg packages)
    (let ((oldtags (lpe::package->tags pkg)))
      (when (not add)
        (dolist (tag oldtags)
          (setf (lpe::package->tags pkg) nil)
          (setf (lpe::tag->packages tag)
                (remove pkg (lpe::tag->packages tag)))))

      (dolist (tag taglist)
        (lpe::tag-package (downcase (s-trim tag)) pkg))
      (lpe::process-line))))


;;;;  Line hiding


(defun lpe::overlay-new (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'evaporate t)
    (push ov lpe::*overlays*)
    ov))

(defvar lpe::*hidden-entries*)

(defun lpe::hide-package (pkg)
  (setq tabulated-list-entries (remove-if (lambda (entry)
                                            (eq pkg (package-desc-name (car entry))))
                                          tabulated-list-entries)))
(defun lpe::hide-line ()
  (lpe::hide-package (lpe::package-at-point)))

(defun lpe::show-all-lines ()
  (interactive)
  (revert-buffer))


;;;;  Filters

(defvar lpe::*filters-history* nil)

(setq lpe::*filters-history-pos* 0)

(defstruct (lpe::filter (:constructor lpe::make-filter)
                        (:copier lpe::copy-filter))
  function
  string)

(defun lpe::set-filter (filter-fn filter-str)
  (push (lpe::make-filter :function filter-fn
                          :string filter-str)
        lpe::*filters-history*)
  (setq lpe::*filters-history-pos* 0))


(defun lpe::current-filter ()
  (nth lpe::*filters-history-pos* lpe::*filters-history*))


(defun lpe::current-filter-function ()
  (and (lpe::current-filter)
       (lpe::filter-function (lpe::current-filter))))


(defun lpe::current-filter-string ()
  (or (and (lpe::current-filter)
           (lpe::filter-string (lpe::current-filter)))
      ""))

;; filter syntax:
;; tag1,tag2,tag3,!tag4/tag5 = tag1 and tag2 and tag3 and (not tag4) or tag5
(defun lpe::parse-filter (filter-str)
  (let* ((tag-sets-strings (s-split "/" filter-str))
         (tag-sets (mapcar (lambda (tss)
                             (let* ((tags (s-split "," tss))
                                    (required-tags (remove-if (lambda (tag)
                                                                (s-starts-with? "!" tag))
                                                              tags))
                                    (required-absent-tags (set-difference tags required-tags
                                                                          :test 'equal)))
                               (cons required-tags required-absent-tags)))
                           tag-sets-strings)))
    (lambda (_package-desc tags)
      (some (lambda (tagset)
              ;; the package must match all required tags, and no tag
              ;; in package must be required-basent
              (and (every (lambda (tag)
                            (member tag tags))
                          (car tagset))
                   (every (lambda (tag)
                            (not (member tag (cdr tagset))))
                          tags)))
            tag-sets))))


(defun lpe::regex-filter (regex)
  (lambda (package-desc _tags)
    (or (s-match regex (symbol-name (package-desc-name package-desc)))
        (and lpe::search-in-summary
             (s-match regex (package-desc-summary package-desc))))))




;;;;  Buffer processing


(let ((current-progress 0)
      (progress-message ""))

  (defun lpe::start-progress (message)
    (setq progress-message message)
    (setq current-progress 0))

  (defun lpe::update-progress ()
    (let* ((n (round (* 33 (/ (float (point)) (point-max))))))
      (when (/= n current-progress)
        (setq current-progress n)
        (message (format "%s |%-33s|"
                         progress-message
                         (concat (s-repeat n "=") ">")))))))


(defun lpe::process-buffer ()
    (lpe::show-all-lines)
    (lpe::start-progress  "Filtering: ")
    (let (to-hide)
      (dolist (entry tabulated-list-entries)
        (let* ((pkg-desc (car entry))
               (pkg (package-desc-name pkg-desc))
               (tags (lpe::package->tags pkg)))
          (lpe::update-progress)
          (when (or (and (lpe::current-filter-function) (not (funcall (lpe::current-filter-function) pkg-desc tags)))
                    (and (not lpe::*show-hidden-p*) (member "hidden" tags)))
            (push entry to-hide))))
      (setq tabulated-list-entries (set-difference tabulated-list-entries to-hide :test 'equal))))


(defun lpe::process-line ()
  (let ((tags (lpe::tags-at-point)))
    (when (or (and (lpe::current-filter-function) (not (funcall (lpe::current-filter-function) (lpe::package-desc-at-point) tags)))
              (and (not lpe::*show-hidden-p*) (member "hidden" tags)))
      ;; Saving position to avoid going back to beginning of buffer
      (let ((next-pos (line-beginning-position 2)))
        (lpe::hide-line)
        (goto-char next-pos))
      (tabulated-list-print t))))



;;;;  Minibuffer
;;; TODO: provide long/short info


(defun lpe::update-minibuffer-info()
  (interactive)
  (message (format "%s\n%s\n%s" ;; "%-40s | %-40s | %-40s"
                   (lpe::format-tags)
                   (lpe::format-filter)
                   (lpe::format-last-applied-tags))))


(defun lpe::filter-type ()
  (concat (cond ((s-starts-with? "/" (lpe::current-filter-string))
                 (concat "Names" (if lpe::search-in-summary "/Summary" "")))
                ((null (lpe::current-filter-function))
                 "All")
                (t "Tags"))
          (if lpe::*show-hidden-p* "+Hidden" "")))


(defun lpe::format-filter ()
  (format "%-25s %40s"
          (propertize (format "Filter[%s]: " (lpe::filter-type))
                      'face '((:foreground "dodger-blue")))
          (propertize (lpe::current-filter-string)
                      'face '((:foreground "dim grey")))))


(defun lpe::format-tags ()
  (format "%-25s%40s"
          (propertize "Tags: " 'face '((:foreground "green")))
          (s-join "," (lpe::tags-at-point))))

(defun lpe::format-last-applied-tags ()
  (format "%-25s %40s"
          (propertize "Apply with .: " 'face '((:foreground "Red")))
          (s-join "," lpe::*last-applied-tags*)))


;;;;  User commands

;;; Tagging

(defun* lpe:tag (taglist &optional add)
  (interactive (let ((add-mode-p (or (and current-prefix-arg
                                          (not (region-active-p)))
                                     (and (not current-prefix-arg)
                                          (region-active-p)))))

                      (list (completing-read-multiple
                             (apply 'format "%s tags (comma separated%s): "
                                    (if add-mode-p
                                        (list "Modify" ", prepend with `!' to remove a tag")
                                      (list "Set" "")))
                             (lpe::all-tags)
                             nil nil
                             (if add-mode-p
                                 nil
                               (s-join "," (lpe::package->tags
                                            (lpe::package-at-point)))))
                            add-mode-p)))
  (assert (or add (every (lambda (tag) (not (s-starts-with? "!" tag)))
                         taglist))
          nil
          "Tag names cannot start with !")

  (lpe::tag% taglist (if (region-active-p)
                         (lpe::packages-in-region (region-beginning)
                                                  (region-end))
                       (list (lpe::package-at-point)))
             add)
  (lpe::save-state)
  (lpe::update-minibuffer-info))


(defun lpe:apply-last-tags ()
  (interactive)
  (lpe:tag lpe::*last-applied-tags*))


(defun lpe:show-hidden-toggle ()
  (interactive)
  (setq lpe::*show-hidden-p* (not lpe::*show-hidden-p*))
  (lpe::update-all))


;;; filtering

(defun lpe:filter (filter-str)
  (interactive "sFilter (tag expression): ")
  (cond ((s-blank? filter-str)
         (lpe::show-all-lines)
         (lpe::set-filter nil "None")
         (lpe::update-minibuffer-info))
        (t
         (lpe::set-filter (lpe::parse-filter filter-str) filter-str)
         (lpe::update-all))))


(defun lpe:filter-with-regex (regex)
  (interactive "sFilter (regex): ")
  (lpe::set-filter (lpe::regex-filter regex) (format "/%s/" regex))
  (lpe::update-all))


(defun lpe:search-in-summary-toggle ()
  (interactive)
  (setq lpe::search-in-summary
        (not lpe::search-in-summary))
  (lpe::update-all))


(defun lpe:filters-history-forward ()
  (interactive)
  (if (zerop lpe::*filters-history-pos*)
      (error "Already at the newest filter.")
    (decf lpe::*filters-history-pos*)
    (lpe::update-all)))


(defun lpe:filters-history-backward ()
  (interactive)
  (if (>= lpe::*filters-history-pos* (1- (length lpe::*filters-history*)))
      (error "End of history.")
    (incf lpe::*filters-history-pos*)
    (lpe::update-all)))


;;;;  Post command hook


(defun lpe::post-command-hook ()
  (when (and (eq major-mode 'package-menu-mode)
             list-packages-ext-mode
             (s-blank? (current-message)))
    (lpe::update-minibuffer-info)))


;;;;  Kludges

(eval-after-load 'smooth-scrolling
  (progn
    (defun disable-smooth-scroll ()
      (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
      (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
      (ad-activate 'next-line)
      (ad-activate 'previous-line))


    (add-hook 'list-packages-ext-mode-hook
              'disable-smooth-scroll)))


;;;;  Keybindings

(flet ((dk (kblist)
           (dolist (kbdef kblist)
             (define-key
               list-packages-ext-mode-map
               (kbd (car kbdef))
               (cadr kbdef)))))
  (dk '(("t" lpe:tag)
        ("f" lpe:filter)
        ("F" lpe:filter-with-regex)
        ("H" lpe:show-hidden-toggle)
        ("v" lpe:search-in-summary-toggle)
        ("g" (lambda () (interactive)
               (revert-buffer)
               (lpe::update-all)))
        ("<XF86Back>" lpe:filters-history-backward)
        ("<XF86Forward>" lpe:filters-history-forward)
        ("<M-right>" lpe:filters-history-forward)
        ("<M-left>" lpe:filters-history-backward)
        ("." lpe:apply-last-tags)
        ("k"
         (lambda ()
           (interactive)
           (lpe:tag '("hidden") t)))

        ("C"
         (lambda ()
           (interactive)
           (when (yes-or-no-p "Are you sure you want to clear all the tags? ")
             (lpe::clear-tags)
             (lpe::update-all)))))))


(provide 'list-package-ext)
;;; list-package-extras.el ends here
