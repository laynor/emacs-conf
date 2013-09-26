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


;;;; Tagging subsystem

(defvar lpe::*basic-tags* '(append ("hidden" "starred") finder-known-keywords)
  "Default tags that will be proposed to the user when tagging a
package. Used for completion.
See `lpe::all-tags'."
  )

(defvar lpe::*tag->objects* (ht-create)
  "Table used to maintain tag->objet associations. Superfluous,
  evaluating removal of this variable. Can speed up some operations.")

(defvar lpe::*object->tags* (ht-create)
  "Table used to maintain object->tags associations")

(defvar lpe:*tag-expr-and*  ","
  "AND operator representation in tag filters. See `lpe::read-tags'.")

(defvar lpe:*tag-expr-or*   "/"
  "OR operator representation in tag filters. See `lpe::read-tags'.")

(defvar lpe:*tag-expr-not*  "!"
  "NOT operator representatin in tag filters. See `lpe::read-tags'.")

;;; TODO: remove global variables
(cl-defstruct (lpe::tag-support (:constructor lpe::make-tag-support)
                                (:copier lpe::copy-tag-support))
  object->tags
  tag->objects
  and-op
  or-op
  not-op
  basic-tags)


(defun lpe::clear-tags ()
  "Clears the tags."
  (setq lpe::*tag->objects* (ht-create))
  (setq lpe::*object->tags* (ht-create)))


(defun lpe::save-tags ()
  "Saves the tags to `lpe::*cache-location*'."
  (persistent-soft-store 'lpe::*tag->objects*
                         (ht-to-alist lpe::*tag->objects*)
                         lpe::*cache-location*)
  (persistent-soft-store 'lpe::*object->tags*
                         (ht-to-alist lpe::*object->tags*)
                         lpe::*cache-location*))


(defun lpe::restore-tags ()
  "Restores the tags from disk."
  (let ((t->p (persistent-soft-fetch 'lpe::*tag->objects* lpe::*cache-location*))
        (p->t (persistent-soft-fetch 'lpe::*object->tags* lpe::*cache-location*)))
    (setq lpe::*tag->objects* (or (and t->p (ht-from-alist t->p)) (ht-create)))
    (setq lpe::*object->tags* (or (and p->t (ht-from-alist p->t)) (ht-create)))))


(defun lpe::tag-negated? (tag)
  "Returns true if TAG is negated."
  (s-starts-with? lpe:*tag-expr-not* tag))

(defun lpe::negate-tag (tag)
  "Returns the negation of TAG for use in a tag expression."
  (if (lpe::tag-negated? tag)
      (lpe::tag-abs tag)
    (concat lpe:*tag-expr-not* tag)))

(defun lpe::tag-abs (tag)
  "Returns the absolute value of tag, such that
   (equal (lpe::tag-abs tag)  (lpe::tag-abs (lpe::negate-tag tag)))
holds."
  (replace-regexp-in-string (format "^\\(%s\\)?" lpe:*tag-expr-not*) "" tag))

(defun lpe::tag-match? (tag-1 tag-2)
  "Returns true if
   (equal (lpe::tag-abs tag-1) (lpe::tag-abs tag-2))
holds."
  (let ((t1 (lpe::tag-abs tag-1))
        (t2 (lpe::tag-abs tag-2)))
    (equal t1 t2)))


(defun lpe::tag->objects (tag)
  "Returns the objects tagged with TAG."
  (ht-get lpe::*tag->objects* tag))

(gv-define-setter lpe::tag->objects (val tag)
  `(ht-set lpe::*tag->objects* ,tag ,val))


(defun lpe::tags-of (object)
  "Returns the the tags of OBJECT as a list."
  (ht-get lpe::*object->tags* object))

(gv-define-setter lpe::tags-of (val object)
  `(ht-set lpe::*object->tags* ,object ,val))


(defun lpe::all-tags ()
  "Returns the list of all the tags used, plus the set of basic
tags `lpe::*basic-tags*'."
  (cl-union lpe::*basic-tags*
            (ht-keys lpe::*tag->objects*) :test 'equal))

(defun lpe::has-tag? (object tag)
  "Returns non nil if OBJECT is tagged with TAG."
  (member tag (lpe::tags-of object)))


(defun lpe::tag-add (object tag)
  "Tags OBJECT with TAG."
  (pushnew tag (lpe::tags-of object) :test 'equal)
  (pushnew object (lpe::tag->objects tag) :test 'equal))


(defun lpe::tag-remove (object tag)
  "Removes TAG from OBJECT."
  (let ((tag (lpe::tag-abs tag)))
    (setf (lpe::tags-of object) (cl-remove (lpe::tag-abs tag) (lpe::tags-of object)
                                           :test 'equal))
    (setf (lpe::tag->objects tag) (cl-remove object (lpe::tag->objects tag)
                                             :test 'equal))))

(defun lpe::tag-toggle (object tag)
  "Toggles the presence of TAG in tag-set the tag-set of OBJECT."
  (if (lpe::has-tag? object tag)
      (lpe::tag-remove object tag)
    (lpe::tag-add object tag)))


(defun lpe::tag-object (tag object &optional toggle)
  "If TOGGLE is non nil, toggles the presence of TAG in the
tag-set of OBJECT, otherwise adds TAG to object or removes it
negated."
  (assert (not (and (lpe::tag-negated? tag) toggle))
          nil
          "Negated tags cannot be toggled.")
  (cond (toggle
          (lpe::tag-toggle object tag))
         ((lpe::tag-negated? tag)
          (lpe::tag-remove object tag))
         (t (lpe::tag-add object tag ))))


;;; FIXME This crappy fest of global variables oughts to be encapsulated
(defvar lpe:*all-tags* nil)

(defvar lpe::*accept-negated-tags* nil)

(defun lpe::complete-tags (string)
  "Completes a tag sequence separated with `lpe:*tag-expr-and*'."
  (let* ((tags (s-split lpe:*tag-expr-and* string))
         (last-tag (car (last tags)))
         (other-tags (butlast tags))
         (!last-tag (lpe::tag-negated? last-tag))
         (all-tags (if !last-tag
                       (mapcar 'lpe::negate-tag lpe:*all-tags*)
                     lpe:*all-tags*)))

    (assert (or (and !last-tag lpe::*accept-negated-tags*)
                (not !last-tag))
            nil
            (format "Tags cannot start with `%s', and negated tags are not acceptable in this context."
                    lpe:*tag-expr-not*
            ))

    (cl-flet ((format-completion (tag)
                (let ((full-completion (s-join lpe:*tag-expr-and* (append other-tags (list tag))))
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
                (and (not (cl-find tag (butlast other-tags) :test 'lpe::tag-match?))
                     (s-starts-with? last-tag tag))))

      (mapcar #'format-completion
              (cl-remove-if-not #'string-matches
                                all-tags)))))

(defun lpe::read-tags (prompt initial-input &optional accept-negated-p)
  "Prompts the user with PROMPT and INITIAL-INPUT for a tag
  sequence separated by `lpe:*tag-expr-and*'. If ACCEPT-NEGATED-P
  is non nil, negated tags are accepted."

  (let ((lpe::*accept-negated-tags* accept-negated-p)
        (lpe:*all-tags* (lpe::all-tags)))
    (completing-read prompt
                     (completion-table-dynamic 'lpe::complete-tags)
                     nil nil initial-input)))

;;;; Tag based filters

;;; Tag expressions:
;;; Filters are expressed as logic predicates, in disjunctive form
;;; AND: ,
;;; OR:  /
;;; NOT: !
;;;
;;; Example:
;;;  tag1,tag2,tag3,!tag4/tag5 = tag1 and tag2 and tag3 and (not tag4) or tag5

(defun lpe::tag-expr-parse (tag-expr)
  "Parses a tag expression in disjunctive (sum of products) form.
For an overview of the syntax, see `lpe::tag-expr-read'."
  (let* ((tag-sets-strings (s-split lpe:*tag-expr-or* tag-expr))
         (tag-sets (mapcar (lambda (tss)
                             (let* ((tags (s-split lpe:*tag-expr-and* tss))
                                    (required-tags (cl-remove-if 'lpe::tag-negated? tags))
                                    (required-absent-tags (cl-remove-if-not 'lpe::tag-negated?
                                                                            tags)))
                               (cons required-tags required-absent-tags)))
                           tag-sets-strings)))
    (lambda (tags)
      (cl-some (lambda (tagset)
                 ;; the package must match all required tags, and no tag
                 ;; in package must be required-basent
                 (and (cl-every (lambda (tag)
                                  (member tag tags))
                                (car tagset))
                      (cl-notany (lambda (tag)
                                   (cl-find-if (lambda (negated-tag)
                                                 (lpe::tag-match? tag negated-tag))
                                               (cdr tagset)))
                                 tags)))
               tag-sets))))


;;;;  Tags Completion


(defun lpe::tag-expr-completions (string)
  (let* ((tag-groups (s-split lpe:*tag-expr-or* string))
         (last-tag-group (s-split lpe:*tag-expr-and* (car (last tag-groups))))
         (last-tag (car (last last-tag-group)))
         (completed-text (s-join lpe:*tag-expr-or*
                                 (append (butlast tag-groups)
                                         (list (s-join lpe:*tag-expr-and*
                                                       (butlast last-tag-group)))))))
    (cl-flet ((propertize-completion (tag)
                (let ((full-completion
                       (concat completed-text
                               (unless (or (s-blank? completed-text)
                                           (s-ends-with? lpe:*tag-expr-or* completed-text))
                                 lpe:*tag-expr-and*)
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
                (and (null (cl-find tag (butlast last-tag-group) :test 'lpe::tag-match?))
                     (s-starts-with? last-tag tag))))


      (mapcar #'propertize-completion
              (cl-remove-if-not #'string-matches-tag
                                (if (lpe::tag-negated? last-tag)
                                    (mapcar #'lpe::negate-tag lpe:*all-tags*)
                                  lpe:*all-tags*))))))


(defun lpe::tag-expr-read (prompt &optional collection initial-input)
  "Reads a tag expression in disjunctive (sum of products) form.
A tag filter like
  (tag1 AND tag2 AND NOT tag3) or tag4
is expressed as
  tag1,tag2,!tag3/tag4
The syntax for the operators can be controlled binding
`lpe::*tag-expr-and*', `lpe::*tag-expr-or*' and `lpe::*tag-expr-not*' "
  (let ((lpe:*all-tags* (or collection (lpe::all-tags))))
    (completing-read prompt
                     (completion-table-dynamic 'lpe::tag-expr-completions)
                     nil
                     nil
                     initial-input)))


;;;;  Notes subsystem
(defvar lpe::*package->notes* (ht-create)
  "Hash table containing package->notes associations.")

(defmacro lpe::package->notes (package)
  "Returns the notes for PACKAGE as a string. SETF-able."
  `(gethash ,package lpe::*package->notes*))


;;; XXX other ugly variable that oughts to be encapsulated
(defvar lpe:*package* nil)

(defun lpe:edit-package-notes (package)
  "Opens a buffer where the user can enter notes about PACKAGE."
  (interactive (list (lpe::package-at-point)))
  (let ((buf (get-buffer-create (format "*Notes for %s*" package))))
    (with-current-buffer buf
      (erase-buffer)
      (let ((notes (lpe::package->notes package)))
        (when notes
          (insert notes)))
      (org-mode)
      (setq lpe:*package* package)
      (local-set-key (kbd "C-c C-c") 'lpe::save-package-notes ))
    (switch-to-buffer-other-window buf)))

(defun lpe::save-package-notes  ()
  "Saves the notes on disk."
  (interactive)
  (let ((notes (buffer-string)))
    (cond ((s-blank? notes)
           (ht-remove lpe::*package->notes* lpe:*package*))

          (t (setf (lpe::package->notes lpe:*package*)
                   notes)
             (lpe::save-state))))
  (set-buffer-modified-p nil)
  (kill-buffer)
  (switch-to-buffer-other-window "*Packages*"))


;;;;  Variables and faces



;;; Package properties

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
  (lpe::save-tags)
  (persistent-soft-store 'lpe::*package->notes*
                         (ht-to-alist lpe::*package->notes*)
                         lpe::*cache-location*))


(defun lpe::resume-state ()
  (lpe::restore-tags)
  (let ((p->n (persistent-soft-fetch 'lpe::*package->notes* lpe::*cache-location*)))
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
  (lpe::tags-of (lpe::package-at-point)))


(defun* lpe::tag% (taglist packages add &optional toggle)
  (setf lpe::*last-applied-tags* taglist)
  (dolist (pkg packages)
    (let ((oldtags (lpe::tags-of pkg)))
      (unless (or add toggle)
        (dolist (tag oldtags)
          (setf (lpe::tags-of pkg) nil)
          (setf (lpe::tag->objects tag)
                (remove pkg (lpe::tag->objects tag)))))

      (dolist (tag taglist)
        (lpe::tag-object (downcase (s-trim tag)) pkg toggle)))))



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
  (lpe::has-tag? package "hidden"))


;;;;  Starring

(defvar lpe:*star-fringe-bitmap* 'filled-rectangle)

(defvar lpe::*package->overlay* (ht-create))

(defun lpe::package->overlay (package)
  (ht-get lpe::*package->overlay* package))

(gv-define-setter lpe::package->overlay (val package)
  `(ht-set lpe::*package->overlay* ,package ,val))


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
  (lpe::has-tag? package "starred"))


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


;;;;  Buffer processing


(defun lpe::process-table ()
  (lpe::show-all-lines)
  (let* ((pr (make-progress-reporter "Filtering ..." 0 (length tabulated-list-entries)))
         to-hide
         (i 0))
    (dolist (entry tabulated-list-entries)
      (let* ((pkg-desc (car entry))
             (pkg (package-desc-name pkg-desc))
             (tags (lpe::tags-of pkg)))
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

    (ignore-errors (delete-overlay (lpe::package->overlay pkg)))

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
  (interactive (let* ((add-mode-p (or (and current-prefix-arg
                                           (not (region-active-p)))
                                      (and (not current-prefix-arg)
                                           (region-active-p))))
                      (prompt (apply 'format "%s tags (comma separated%s): "
                                     (if add-mode-p
                                         (list "Modify" ", prepend with `!' to remove a tag")
                                         (list "Set" "")))))
                 (list (s-split "," (lpe::read-tags prompt
                                                    (unless add-mode-p
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

(defun lpe:filter-by-tag-expr (filter-str)
  (interactive (list (lpe::tag-expr-read "Filter (tag expression): ")))
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
         (lpe::set-filter (let ((tagfilter (lpe::tag-expr-parse filter-str)))
                            (lambda (_package-desc tags)
                              (funcall tagfilter tags)))
                          filter-str)
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
        ("f" lpe:filter-by-tag-expr)
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
