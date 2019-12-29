;;; doct.el --- DOCT: Declarative Org capture templates -*- lexical-binding: t; -*-
;; Author: nv <progfolio@protonmail.com>
;; URL: https://github.com/progfolio/doct
;; Created: December 10, 2019
;; Keywords: org, convenience
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.3

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides an alternative syntax for declaring Org capture
;; templates. See the doctstring for doct for more details.

;;; Code:

(defgroup doct nil
  "DOCT: Declarative Org Capture Templates"
  :group 'org)

(defcustom doct-default-entry-type 'entry
  "The default template entry type.
Can be overridden by using the :type keyword in a declarative form."
  :type 'symbol
  :options '(entry item checkitem table-line plain)
  :group 'doct)

(defcustom doct-after-conversion-hook nil
  "Hook run after doct has converted declarative forms to templates.
Hook functions are run with the list of templates as their only argument.
The templates have not been flattened at this point and are of the form:
\(((parent) (child)...)...)."
  :group 'doct
  :type 'hook)

(defvar doct-templates nil
  "If non-nil, this is used as the return value of doct.
Use this variable to return an altered list from a function run during
`doct-after-conversion-hook'
Its value is not stored betewen invocations to doct.")

(defvar doct--current-form nil
  "The current form being processed by doct. Used for error processing.")

(defvar doct-option-keywords '(:clock-in
                               :clock-keep
                               :clock-resume
                               :empty-lines
                               :empty-lines-after
                               :empty-lines-before
                               :immediate-finish
                               :jump-to-captured
                               :kill-buffer
                               :no-save
                               :prepend
                               :table-line-pos
                               :time-prompt
                               :tree-type
                               :unnarrowed)
  "Keywords that define a template's additional options.")

(defvar doct-file-extension-keywords '(:datetree :function :headline :olp :regexp)
  "Keywords that define the insertion location in the target file.")

(defvar doct-exclusive-location-keywords '(:clock :file :function :id)
  "Keywords that exclusively set the target location.")

(defvar doct-hook-keywords '(:after-finalize :before-finalize :hook :prepare-finalize)
  "Keywords that attach hooks for the current template.")

(defvar doct-template-keywords '(:template :template-file :template-function)
  "Keywords that define the template string.")

(defvar doct-recognized-keywords `(:children
                                   :doct--parent
                                   :keys
                                   :type
                                   ,@(append
                                      ;;:function is in two categories
                                      ;;only need to add once
                                      (remq :function
                                            doct-file-extension-keywords)
                                      doct-exclusive-location-keywords
                                      doct-hook-keywords
                                      doct-template-keywords
                                      doct-option-keywords))
"List of the keywords doct recognizes.")

(define-error 'doct-no-keys "Form has no :keys value" 'doct-error)
(define-error 'doct-wrong-type-argument "Wrong type argument" 'doct-error)
(define-error 'doct-malformed-parent "Parent entry is malformed" 'doct-error)
(define-error 'doct-no-target "Form has no target" 'doct-error)
(define-error 'doct-no-template "Form has no template" 'doct-error)

(defun doct--first-in-plist (plist keywords)
  "Find first occurence of one of KEYWORDS in PLIST."
  (car (seq-some (lambda (property)
                   (and (keywordp property) (member property keywords)))
                 plist)))

(defun doct-remove-hooks (&optional keys hooks unintern-functions)
  "Remove hooks matching KEYS from HOOKS.
doct hook functions follow the form:

  doct--hook/<org-capture-hook-variable-name>/KEYS.

KEYS is a regular expression which matches KEYS in the above form.

HOOKS is one of five symbols:
  after-finalize
    removes matching functions from `org-capture-after-finalize-hook'
  before-finalize
    removes matching functions from `org-capture-before-finalize-hook'
  prepare-finalize
    removes matching functions from `org-capture-prepare-finalize-hook'
  mode
    removes matching functions from `org-capture-mode-hook'
  t
    removes matching functions from all of the above hooks.

Or a list including any combination of the first four symbols. e.g.

\\='(after-finalize before-finalize mode prepare-finalize)

is equivalent to passing t as HOOKS.

If UNINTERN-FUNCTIONS is non-nil, the matching functions are uninterned.

Example:

  (doct-remove-hooks \"t.?\" \\='mode t)

Removes:

  doct--hook/mode/t
  doct--hook/mode/tt

But not:

  doct--hook/mode/p

From the `org-capture-mode-hook'."
  (interactive)
  (let* ((keys (or keys (read-string "Remove hooks with keys matching: ")))
         ;;only want to match against the end of the hook function's name
         (keys (if (string= (substring keys -1) "$")
                   keys
                 (concat keys "$")))
         (hook-symbols (pcase hooks
                         ((pred (not)) (mapcar #'intern-soft
                                               (completing-read-multiple
                                                "Hooks: " '("after-finalize"
                                                            "before-finalize"
                                                            "mode"
                                                            "prepare-finalize"
                                                            "t"))))
                         ((pred (lambda (hook)
                                  (memq hook '(after-finalize
                                               before-finalize
                                               mode
                                               prepare-finalize
                                               t))))
                          (list hooks))
                         ((and (pred listp) hooks
                               (guard (seq-every-p 'symbolp hooks)))
                          hooks)
                         (_ (user-error "Unrecognized HOOKS value %s" hooks))))
         (hooks (if (memq t hook-symbols)
                    '(org-capture-after-finalize-hook
                      org-capture-before-finalize-hook
                      org-capture-mode-hook
                      org-capture-prepare-finalize-hook)
                  (mapcar (lambda (symbol)
                            (intern-soft (concat "org-capture-"
                                                 (symbol-name symbol)
                                                 "-hook")))
                          hook-symbols))))
    (dolist (hook hooks)
      (dolist (hook-fn (symbol-value hook))
        (when (string-match keys (symbol-name hook-fn))
          (remove-hook hook hook-fn))
        (when unintern-functions (unintern hook-fn nil))))))


(defun doct--add-hook (keys fn where &optional entry-name)
  "Generate hook function and add to appropriate hook variable.
The generated hook function takes the form \"doct--hook/WHERE/KEYS\".
FN is called when an org-capture-template's keys match KEYS.
WHERE is one of the following strings:
\"mode\"
\"after-finalize\"
\"before-finalize\"
\"prepare-finalize\"
ENTRY-NAME is the name of the entry the hook should run for."
  (if-let ((wrapper (intern (string-join `("doct--hook" ,where ,keys) "/")))
           (hook (intern-soft (concat "org-capture-" where "-hook"))))
      (progn
        (eval `(defun ,wrapper ()
                 ,(string-join
                   `("Auto generated by `doct--add-hook'."
                     ,(concat "It is run as part of `" (symbol-name hook) "'"
                              (if entry-name
                                  (concat " when the \"" entry-name
                                          "\" template is selected.")
                                "'."))
                     "It can be removed using `doct-remove-hooks' like so:"
                     ,(concat "(doct-remove-hooks \""
                              keys "\" \\='" where " t)"))
                   "\n")
                 (when (string= ,keys (plist-get org-capture-plist :key))
                   (funcall (quote ,fn)))))
        (add-hook hook wrapper))
    (error "Doct: couldnt add %s as doct--hook/%s to %s" fn keys where)))

(defun doct--keys (form)
  "Prepend each of FORM's ancestors's keys to its keys."
  (if-let ((keys (plist-get form :keys)))
      (let (inherited-keys parent)
        (unless (stringp keys)
          (signal 'doct-wrong-type-argument `(stringp ,keys
                                                      ,doct--current-form)))
        (while (setq parent (plist-get form :doct--parent))
          (push (plist-get parent :keys) inherited-keys)
          (setq form parent))
        (concat (string-join inherited-keys) keys))
    (signal 'doct-no-keys `(,doct--current-form))))


(defun doct--target-file (form)
  "Convert declarative FORM's :file and file-extensions to Org capture template syntax."
  (let (target-type target-args)
    (pcase (doct--first-in-plist form
                                 ;;datetree is only used when :olp is specified
                                 (remq :datetree doct-file-extension-keywords))
      (:olp (let ((path (plist-get form :olp)))
              (unless (and (listp path)
                           (seq-every-p 'stringp path))
                (signal 'doct-wrong-type-argument
                        `((stringp) ,path ,doct--current-form)))
              (push (plist-get form :datetree) target-type)
              (push :olp target-type)
              (dolist (heading (nreverse path))
                (push heading target-args))))
      (extension (push extension target-type)
                 (push (plist-get form extension)
                       target-args)))
    (push :file target-type)
    (push (plist-get form :file) target-args)
    `(,(intern (string-join
                (mapcar (lambda (keyword)
                          (substring (symbol-name keyword) 1))
                        (delq nil target-type)) "+"))
      ,@(delq nil target-args))))

(defun doct--target (form)
  "Convert declarative FORM's target to Org captures target."
  (pcase (doct--first-in-plist form doct-exclusive-location-keywords)
    ('nil (signal 'doct-no-target `(,doct-exclusive-location-keywords
                                    nil ,doct--current-form)))
    (:clock '(clock))
    (:id `(id ,(plist-get form :id)))
    (:function (unless (plist-get form :file)
                 `(function ,(plist-get form :function))))
    (:file (doct--target-file form))))

(defun doct--template (form)
  "Convert FORM's template target to Org capture template syntax."
  (pcase (doct--first-in-plist form doct-template-keywords)
    ('nil (signal 'doct-no-template `(,doct-template-keywords
                                      nil ,doct--current-form)))
    (:template
     (setq template (let ((template (plist-get form :template)))
                      (unless (or (stringp template)
                                  (and (listp template)
                                       (seq-every-p 'stringp template)))
                        (signal 'doct-wrong-type-argument
                                `((stringp listp)
                                  ,template ,doct--current-form)))
                      (if (stringp template)
                          template
                        (string-join template "\n")))))
    (:template-file
     (setq template `(file ,(plist-get form :template-file))))
    (:template-function
     (setq template
           `(function ,(plist-get form :template-function))))))

(defun doct--additional-args (form)
  "Convert FORM's additional args to Org capture syntax.
Returns a list of ((ADDITIONAL OPTIONS) (UNRECOGNIZED ARGS))."
  (let (additional-options unrecognized-args)
    (dolist (keyword (delete-dups (seq-filter 'keywordp form)))
      (when (plist-get form keyword)
        (pcase keyword
          ((pred (lambda (keyword) (member keyword doct-option-keywords)))
           (push keyword additional-options)
           (push (plist-get form keyword) additional-options))
          ((pred (lambda (keyword)
                   (not (member keyword doct-recognized-keywords))))
           (push keyword unrecognized-args)
           (push (plist-get form keyword) unrecognized-args)))))
    `(,(nreverse additional-options) ,(nreverse unrecognized-args))))

(defun doct--add-hooks (name form keys)
  "Add hooks declared in FORM for template NAME with KEYS."
  (dolist (keyword doct-hook-keywords)
    (when-let ((hook-fn (plist-get form keyword))
               (hook (if (eq keyword :hook) "mode"
                       ;;remove preceding ':' from keyword
                       (substring (symbol-name keyword) 1))))
      (doct--add-hook keys hook-fn hook name))))


(defun doct--parent-p (form)
  "Return t if FORM is a valid parent, nil otherwise."
  (not (seq-filter (lambda (keyword)
                     (not (member keyword '(:children :doct--parent :keys))))
                   (seq-filter 'keywordp form))))

(defun doct--type (form)
  "Return FORM's :type value or `doct-default-entry-type'."
  (let ((type (or (plist-get form :type)
                  doct-default-entry-type))
        (entry-types '(entry item checkitem table-line plain)))
    (unless (member type entry-types)
      (signal 'doct-wrong-type-argument `(,entry-types ,type)))
    type))

(defun doct--convert (name &rest args)
  "Convert declarative form to template named NAME with ARGS.
For a full description of ARGS see `doct'."
  (setq doct--current-form `(,name ,@args))
  (let ((is-parent (doct--parent-p args))
        (children (plist-get args :children))
        (keys (doct--keys args))
        (additional-args
         (doct--additional-args args))
        entry)

    (unless (stringp name)
      (signal 'doct-wrong-type-argument
              `(stringp ,name ,doct--current-form)))

    (when children
      (unless is-parent
        (signal 'doct-malformed-parent `((:keys :children) ,doct--current-form)))
      (setq children (mapcar (lambda (child)
                               (apply #'doct--convert
                                      (append child `(:doct--parent ,args))))
                             (if (not (seq-every-p 'listp children))
                                 `(,children)
                               children))))

    (doct--add-hooks name args keys)

    (setq entry (delq nil `(,keys
                            ,name
                            ,@(unless is-parent
                                `(,(doct--type args)
                                  ,(doct--target args)
                                  ,(doct--template args)
                                  ,@(car additional-args)
                                  ,@(cadr additional-args))))))
    (if is-parent
        `(,entry ,@children)
      entry)))

(defun doct-flatten-lists-in (list-of-lists)
  "Flatten each list in LIST-OF-LISTS.
For example:
  '((1) ((2) (3)) (((4))))
returns:
  '((1) (2) (3) (4))"
  (let (flattend)
    (letrec ((flatten (lambda (list)
                        (dolist (element list)
                          (if (seq-every-p 'listp element)
                              (funcall flatten element)
                            (push element flattend))))))
      (funcall flatten list-of-lists)
      (nreverse flattend))))

;;@FIX needs overview docstring
(defun doct (declarations)
  "DECLARATIONS is a list of declarative forms."
  (let* ((maybe-convert-form
          (lambda (form)
            (condition-case err
                (apply 'doct--convert form)
              (doct-error (user-error (format "DOCT %s"
                                              (error-message-string err)))))))
         (entries (mapcar maybe-convert-form declarations)))

    (unwind-protect
        (progn
          (run-hook-with-args 'doct-after-conversion-hook entries)
          ;;hook functions may set doct-templates to return manipulated list
          (or doct-templates (doct-flatten-lists-in entries)))
      (setq doct-templates nil))))

(provide 'doct)

;;; doct.el ends here
