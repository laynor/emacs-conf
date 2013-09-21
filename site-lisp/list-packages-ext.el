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

;; TODO: undo support
;;

(require 'cl-lib)
(require 's)
(require 'ht)
(require 'persistent-soft)

;;; Code:

;;;;  Macros


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
        (exp* (cl-gensym)))
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



;;;;  Variables and faces


;;; Package properties
(defvar lpe::*tag->packages* (ht-create))
(defvar lpe::*package->tags* (ht-create))
(defvar lpe::*package->notes* (ht-create))

;;; Non persistent state
(defvar lpe::*overlays* nil)
(defvar lpe::*filterfn* nil)
(defvar lpe::*show-hidden-p* nil)
(defvar lpe::*current-filter* "")
(defvar lpe::search-in-summary nil)
(defvar lpe::*last-applied-tags* nil)

;;; Other stuff
(defvar list-packages-ext-mode-map (make-sparse-keymap))
(defvar lpe::*cache-location* "list-packages-ext-mode")

(defface lpe:star-fringe-face
    '((t (:foreground "yellow")))
  "face to fontify Enotify Success messages"
  :group 'package)

(defface lpe:hidden-fringe-face
    '((t (:foreground "MediumPurple3")))
  "face to fontify Enotify Success messages"
  :group 'package)


;;;;  Persistence


(defun lpe::save-state ()
  (persistent-soft-store 'lpe::*tag->packages*
                         (ht-to-alist lpe::*tag->packages*)
                         lpe::*cache-location*)
  (persistent-soft-store 'lpe::*package->tags*
                         (ht-to-alist lpe::*package->tags*)
                         lpe::*cache-location*)
  (persistent-soft-store 'lpe::*package->notes*
                         (ht-to-alist lpe::*package->notes*)
                         lpe::*cache-location*))


(defun lpe::resume-state ()
  (let ((t->p (persistent-soft-fetch 'lpe::*tag->packages* lpe::*cache-location*))
        (p->t (persistent-soft-fetch 'lpe::*package->tags* lpe::*cache-location*))
        (p->n (persistent-soft-fetch 'lpe::*package->notes* lpe::*cache-location*)))
    (setq lpe::*tag->packages* (or (and t->p (ht-from-alist t->p)) (ht-create)))
    (setq lpe::*package->tags* (or (and p->t (ht-from-alist p->t)) (ht-create)))
    (setq lpe::*package->notes* (or (and p->n (ht-from-alist p->n)) (ht-create)))))



;;;;  Minor mode

(define-minor-mode list-packages-ext-mode
    "Some extras for the *Packages* buffer (see `list-packages').
Provides:
- package tagging
- package hiding (with the tag 'hidden'
- package filtering by tag expressions/regexp
- package annotations
\\{list-packages-ext-mode-map}"
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
  (lpe::clear-overlays)
  (remove-hook 'post-command-hook 'lpe::post-command-hook))


(defun lpe::update-all ()
  (lpe::show-all-lines)
  (lpe::process-table)
  (tabulated-list-print t)
  (lpe::update-overlays)
  (lpe::update-minibuffer-info))


;;;;  Tags


(defun lpe::tag-abs (tag)
  (replace-regexp-in-string "^!?" "" tag))


(defun lpe::tag-negated? (tag)
  (s-starts-with? "!" tag))


(defun lpe::tag-match (tag-1 tag-2)
  (let ((t1 (lpe::tag-abs tag-1))
        (t2 (lpe::tag-abs tag-2)))
    (equal t1 t2)))


(defun lpe::clear-tags ()
  (setq lpe::*tag->packages* (ht-create))
  (setq lpe::*package->tags* (ht-create)))


(defun lpe::tag->packages (tag)
  (ht-get lpe::*tag->packages* tag))

(gv-define-setter lpe::tag->packages (val tag)
  `(ht-set lpe::*tag->packages* ,tag ,val))


(defun lpe::package->tags (package)
  (ht-get lpe::*package->tags* package))

(gv-define-setter lpe::package->tags (val package)
  `(ht-set lpe::*package->tags* ,package ,val))


(defun lpe::package-has-tag (package tag)
  (member tag (lpe::package->tags package)))


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
        (goto-char (line-beginning-position 2))))
    packages))

(defun lpe::tags-at-point ()
  (lpe::package->tags (lpe::package-at-point)))


(defun lpe::tag-package (tag package &optional toggle)
  (let* ((packages-with-tag (lpe::tag->packages tag))
         (tags-of-package (lpe::package->tags package))
         (package-has-tag (lpe::package-has-tag tag package)))

    (assert (not (and (lpe::tag-negated? tag) toggle))
            nil
            "Negated tags cannot be toggled.")

    (cond ((or (and toggle package-has-tag) (lpe::tag-negated? tag))
           (let ((tag (lpe::tag-abs tag)))
             (setf tags-of-package (cl-remove (lpe::tag-abs tag) tags-of-package :test 'equal))
             (setf packages-with-tag (cl-remove package packages-with-tag :test 'equal))))

          (t (pushnew tag tags-of-package :test 'equal)
             (pushnew package packages-with-tag :test 'equal)))

    (setf (lpe::tag->packages (lpe::tag-abs tag))
          packages-with-tag)
    (setf (lpe::package->tags package)
          tags-of-package)))


(defun lpe::all-tags ()
  (cl-union (list "hidden" "starred")
            (ht-keys lpe::*tag->packages*) :test 'equal))

(defun* lpe::tag% (taglist packages add &optional toggle)
  (setf lpe::*last-applied-tags* taglist)
  (dolist (pkg packages)
    (let ((oldtags (lpe::package->tags pkg)))
      (unless (or add toggle)
        (dolist (tag oldtags)
          (setf (lpe::package->tags pkg) nil)
          (setf (lpe::tag->packages tag)
                (remove pkg (lpe::tag->packages tag)))))

      (dolist (tag taglist)
        (lpe::tag-package (downcase (s-trim tag)) pkg toggle)))))


;;;;  Line hiding


(defvar lpe::*hidden-entries*)

(defun lpe::hide-package (pkg)
  (setq tabulated-list-entries (cl-remove-if (lambda (entry)
                                               (eq pkg (package-desc-name (car entry))))
                                             tabulated-list-entries)))
(defun lpe::hide-line ()
  (lpe::hide-package (lpe::package-at-point)))

(defun lpe::show-all-lines ()
  (interactive)
  (lpe::clear-overlays)
  (revert-buffer))

(defun lpe::package-hidden? (package)
  (lpe::package-has-tag package "hidden"))


;;;;  Starring

(defvar lpe:*star-fringe-bitmap* 'filled-rectangle)

(defvar lpe::*package->overlay* (ht-create))

(defun lpe::package->overlay (package)
  (ht-get lpe::*package->overlay* package))

(gv-define-setter lpe::package->overlay (val package)
  `(ht-set lpe::*package->overlay* ,package val))


(defun lpe::overlay-new (beg end package &optional face)
  (let ((ov (make-overlay beg end)))
    (setf (lpe::package->overlay package) ov)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'before-string
                 (propertize " " 'display `((left-fringe
                                             ,lpe:*star-fringe-bitmap*
                                             ,(or face 'lpe:star-fringe-face)))))
    (push ov lpe::*overlays*)
    ov))

(defun lpe::update-overlays ()
  (lpe::clear-overlays)
  (save-excursion
    (goto-char (point-min))
    (let ((pkg (lpe::package-at-point)))
      (while pkg
        (let ((bol (line-beginning-position))
              (eol (line-end-position)))
          (cond  ((lpe::package-starred? pkg)
                  (lpe::overlay-new bol eol pkg))
                 ((and (lpe::package-hidden? pkg) lpe::*show-hidden-p*)
                  (lpe::overlay-new bol eol
                                    pkg 'lpe:hidden-fringe-face))))
        (goto-char (line-beginning-position 2))
        (setq pkg (lpe::package-at-point))))))

(defun lpe::clear-overlays ()
  (dolist (ov lpe::*overlays*)
    (delete-overlay ov))
  (setq lpe::*overlays* nil))


(defun lpe::package-starred? (package)
  (lpe::package-has-tag package "starred"))


;;;;  Filters

(defvar lpe::*filters-history* nil)

(setq lpe::*filters-history-pos* 0)

(cl-defstruct (lpe::filter (:constructor lpe::make-filter)
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
                                    (required-tags (cl-remove-if 'lpe::tag-negated? tags))
                                    (required-absent-tags (cl-remove-if-not 'lpe::tag-negated? tags)))                               (cons required-tags required-absent-tags)))
                           tag-sets-strings)))
    (lambda (_package-desc tags)
      (cl-some (lambda (tagset)
                 ;; the package must match all required tags, and no tag
                 ;; in package must be required-basent
                 (and (cl-every (lambda (tag)
                                  (member tag tags))
                                (car tagset))
                      (cl-notany (lambda (tag)
                                   (cl-find-if (lambda (negated-tag)
                                                 (lpe::tag-match tag negated-tag))
                                               (cdr tagset)))
                                 tags)))
               tag-sets))))


(defun lpe::regex-filter (regex)
  (lambda (package-desc _tags)
    (or (s-match regex (symbol-name (package-desc-name package-desc)))
        (and lpe::search-in-summary
             (s-match regex (package-desc-summary package-desc))))))


(defun lpe::packages-with-notes-filter ()
  (lambda (package-desc _tags)
    (let* ((pkg (package-desc-name package-desc))
           (notes (lpe::package->notes pkg)))
      (and notes notes))))


;;;;  Notes

(defmacro lpe::package->notes (package)
  `(gethash ,package lpe::*package->notes*))

(defvar lpe:*package* nil)

(defun lpe:edit-package-notes (package)
  (interactive (list (lpe::package-at-point)))
  (let ((buf (get-buffer-create (format "*Notes for %s*" package))))
    (with-current-buffer buf
      (erase-buffer)
      (let ((notes (lpe::package->notes package)))
        (when notes
          (insert notes)))
      (org-mode)
      (setq lpe:*package* package)
      (local-set-key (kbd "C-c C-c") 'lpe:save-package-notes))
    (switch-to-buffer-other-window buf)))

(defun lpe:save-package-notes ()
  (interactive)
  (let ((notes (buffer-string)))
    (cond ((s-blank? notes)
           (ht-remove lpe::*package->notes* lpe:*package*))

          (t (setf (lpe::*package->notes* lpe:*package*)
                   notes)
             (lpe::save-state))))
  (set-buffer-modified-p nil)
  (kill-buffer)
  (switch-to-buffer-other-window "*Packages*"))


;;;;  Buffer processing


(defun lpe::process-table ()
  (lpe::show-all-lines)
  (let* ((pr (make-progress-reporter "Filtering ..." 0 (length tabulated-list-entries)))
         to-hide
         (i 0))
    (dolist (entry tabulated-list-entries)
      (let* ((pkg-desc (car entry))
             (pkg (package-desc-name pkg-desc))
             (tags (lpe::package->tags pkg)))
        (when (or (and (lpe::current-filter-function)
                       (not (funcall (lpe::current-filter-function)
                                     pkg-desc tags)))
                  (and (not lpe::*show-hidden-p*) (lpe::package-hidden? pkg)))
          (push entry to-hide)))
      (progress-reporter-update pr i)
      (cl-incf i))
    (progress-reporter-done pr)
    (setq tabulated-list-entries (cl-set-difference tabulated-list-entries to-hide
                                                    :test 'equal))))


(defun lpe::process-line ()
  (let ((tags (lpe::tags-at-point))
        ;; Saving position to avoid going back to beginning of buffer
        (next-pos (line-beginning-position 2))
        (pkg (lpe::package-at-point)))

    (ignore-errors (delete-overlay (lpe::package->overlay package)))

    (cond ((or (and (lpe::current-filter-function)
                    (not (funcall (lpe::current-filter-function)
                                  (lpe::package-desc-at-point)
                                  tags)))
               (and (not lpe::*show-hidden-p*)
                    (lpe::package-hidden? pkg)))
           (lpe::hide-line)
           (goto-char next-pos)
           (when (not lpe::*show-hidden-p*)
             (tabulated-list-print t)))

          ((and lpe::*show-hidden-p*
                (lpe::package-hidden? pkg))
           (lpe::overlay-new (line-beginning-position) (line-end-position)
                             pkg
                             'lpe:hidden-fringe-face)
           (goto-char next-pos))

          ((lpe::package-starred? pkg)
           (lpe::overlay-new (line-beginning-position) (line-end-position)
                             (lpe::package-at-point))
           (goto-char next-pos))

          (t (goto-char next-pos)))))



;;;;  Minibuffer
;;; TODO: provide long/short info

(defun lpe::message (&rest args)
  ;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages
  ;; are recorded in a log.  Do not put eldoc messages in that log since
  ;; they are Legion.
  ;; Emacs way of preventing log messages.
  (let ((message-log-max nil))
    (apply 'message args)))


(defun lpe::update-minibuffer-info()
  (interactive)
  (lpe::message "%s\n%s\n%s\n%s" ;; "%-40s | %-40s | %-40s"
                (lpe::format-tags)
                (lpe::format-filter)
                (lpe::format-last-applied-tags)
                (or (ignore-errors
                      (car (s-lines (lpe::package->notes (lpe::package-at-point)))))
                    "")))


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
  (format "%-25s %40s"
          (propertize "Tags: " 'face '((:foreground "green")))
          (s-join "," (lpe::tags-at-point))))


(defun lpe::format-last-applied-tags ()
  (format "%-25s %40s"
          (propertize "Apply with .: " 'face '((:foreground "Red")))
          (s-join "," (or lpe::*last-applied-tags* ""))))


;;;;  User commands

;;; Tagging

(defun* lpe:tag (taglist &optional add)
  (interactive (let ((add-mode-p (or (and current-prefix-arg
                                          (not (region-active-p)))
                                     (and (not current-prefix-arg)
                                          (region-active-p)))))
                 (list (s-split "," (lpe::read-tags (unless add-mode-p
                                                      (s-join "," (lpe::tags-at-point)) )
                                                    add-mode-p))
                       add-mode-p)))

  (assert (or add (not (cl-find-if #'lpe::tag-negated? taglist)))
          nil
          "Tag names cannot start with !")

  (lpe::tag% taglist (if (region-active-p)
                         (lpe::packages-in-region (region-beginning)
                                                  (region-end))
                       (list (lpe::package-at-point)))
             add)
  (lpe::save-state)
  (if (and lpe::*show-hidden-p* (not (region-active-p)))
      (lpe::process-line)
    (unless (region-active-p)
      (goto-char (line-beginning-position 2)))
    (lpe::update-all))
  (lpe::update-minibuffer-info))

(defun lpe:hide-package ()
  (interactive)
  (lpe::tag% '("hidden") (if (region-active-p)
                             (lpe::packages-in-region (region-beginning)
                                                      (region-end))
                           (list (lpe::package-at-point)))
             t t)
  (lpe::save-state)
  (if (and lpe::*show-hidden-p* (not (region-active-p)))
      (lpe::process-line)
    (unless (region-active-p)
      (goto-char (line-beginning-position 2)))
    (lpe::update-all))
  (lpe::update-minibuffer-info))


(defun lpe:apply-last-tags ()
  (interactive)
  (lpe:tag lpe::*last-applied-tags*))


(defun lpe:star ()
  (interactive)
  (lpe::tag% '("starred")
             (if (region-active-p)
                 (lpe::packages-in-region (region-beginning)
                                          (region-end))
               (list (lpe::package-at-point)))
             t t)
  (lpe::save-state)
  (if (region-active-p)
      (lpe::update-all)
    (lpe::process-line))
  (lpe::update-minibuffer-info))


(defun lpe:show-hidden-toggle ()
  (interactive)
  (setq lpe::*show-hidden-p* (not lpe::*show-hidden-p*))
  (lpe::update-all))


(defun lpe:clear-all-tags ()
  (interactive)
  (when (yes-or-no-p "Are you sure you want to clear all the tags? ")
    (lpe::clear-tags)
    (lpe::update-all)))


;;; filtering

(defun lpe:filter (filter-str)
  (interactive (list (lpe::read-tag-expression)))
  (cond ((s-blank? filter-str)
         (lpe::show-all-lines)
         (lpe::set-filter nil "None")
         (lpe::update-all))
        ((s-equals? "with-notes" filter-str)
         (lpe::show-all-lines)
         (lpe::set-filter (lpe::packages-with-notes-filter)
                          "Packages with notes only")
         (lpe::update-all))
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


(defun lpe:refresh ()
  (interactive)
  (revert-buffer)
  (lpe::update-all))



;;;;  Tags Completion

(defvar lpe:*all-tags*)

(defun lpe::complete-tag-expression (string)
  (let* ((tag-groups (s-split "/" string))
         (last-tag-group (s-split "," (car (last tag-groups))))
         (last-tag (car (last last-tag-group)))
         (completed-text (s-join "/" (append (butlast tag-groups)
                                             (list (s-join "," (butlast last-tag-group)))))))
    (cl-flet ((propertize-completion (tag)
                (let ((full-completion
                       (concat completed-text
                               (unless (or (s-blank? completed-text)
                                           (s-ends-with? "/" completed-text))
                                 ",")
                               tag))
                      (displayed-completion (let ((tag (substring tag 0))
                                                  (ll (length last-tag)))
                                              (when (> (length tag) ll)
                                                (set-text-properties
                                                 ll (1+ ll)
                                                 '(face completions-first-difference)
                                                 tag))
                                              tag)))
                  (propertize full-completion 'display displayed-completion)))

              (string-matches-tag (tag)
                (and (null (cl-find tag (butlast last-tag-group) :test 'lpe::tag-match))
                     (s-starts-with? last-tag tag)))

              (add-!-to-completion-if-negated (candidate)
                (concat (if (lpe::tag-negated? last-tag) "!" "")
                        candidate)))

      (mapcar #'propertize-completion
              (cl-remove-if-not #'string-matches-tag
                                (mapcar #'add-!-to-completion-if-negated
                                        lpe:*all-tags*))))))

(defvar lpe:*accept-negation* nil)

(defun lpe::complete-tags (string)
  (let* ((tags (s-split "," string))
         (last-tag (car (last tags)))
         (other-tags (butlast tags))
         (!last-tag (lpe::tag-negated? last-tag))
         (all-tags (if !last-tag
                       (mapcar (lambda (tag)
                                 (s-concat "!" tag))
                               lpe:*all-tags*)
                     lpe:*all-tags*)))
    (assert (or (and !last-tag lpe:*accept-negation*)
                (not !last-tag))
            nil
            "Tags cannot start with `!', and negated tags are not acceptable in this context.")
    (cl-flet ((format-completion (tag)
                (let ((full-completion (s-join "," (append other-tags (list tag))))
                      (displayed-completion (let ((tag (substring tag 0))
                                                  (ll (length last-tag)))
                                              (when (> (length tag) ll)
                                                (set-text-properties
                                                 ll (1+ ll)
                                                 '(face completions-first-difference)
                                                 tag))
                                              tag)))
                  (propertize full-completion 'display displayed-completion)))
              (string-matches (tag)
                (and (not (cl-find tag (butlast other-tags) :test 'lpe::tag-match))
                     (s-starts-with? last-tag tag))))

      (mapcar #'format-completion
              (cl-remove-if-not #'string-matches
                                all-tags)))))

(defun lpe::read-tags (initial-input &optional add-mode-p)
  (interactive)
  (let ((lpe:*accept-negation* add-mode-p)
        (lpe:*all-tags* (lpe::all-tags)))
    (completing-read (apply 'format "%s tags (comma separated%s): "
                            (if add-mode-p
                                (list "Modify" ", prepend with `!' to remove a tag")
                              (list "Set" "")))
                     (completion-table-dynamic 'lpe::complete-tags)
                     nil nil initial-input)))

(defun lpe::read-tag-expression ()
  (interactive)
  (let ((lpe:*all-tags* (lpe::all-tags)))
    (completing-read "Filter (tag expression): "
                     (completion-table-dynamic 'lpe::complete-tag-expression))))


;;;;  Post command hook


(defun lpe::post-command-hook ()
  (when (and (eq major-mode 'package-menu-mode)
             list-packages-ext-mode
             (s-blank? (current-message)))
    (lpe::update-minibuffer-info)))


;;;;  Kludges

;; (eval-after-load 'smooth-scrolling
;;   (progn
;;     (defun disable-smooth-scroll ()
;;       (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
;;       (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
;;       (ad-activate 'next-line)
;;       (ad-activate 'previous-line))


;;     (add-hook 'list-packages-ext-mode-hook
;;               'disable-smooth-scroll)))

(add-hook 'package-menu-mode-hook
          'hl-line-mode)


;;;;  Keybindings

(cl-flet ((dk (kblist)
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
        ("g" lpe:refresh)
        ("<XF86Back>" lpe:filters-history-backward)
        ("<XF86Forward>" lpe:filters-history-forward)
        ("<M-right>" lpe:filters-history-forward)
        ("<M-left>" lpe:filters-history-backward)
        ("s" lpe:star)
        ("." lpe:apply-last-tags)
        ("e" lpe:edit-package-notes)
        ("k" lpe:hide-package)

        ("C" lpe:clear-all-tags))))


(provide 'list-package-ext)
;;; list-package-extras.el ends here
