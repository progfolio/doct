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
(require 'subr-x)
(require 'seq)

(defgroup doct nil
  "DOCT: Declarative Org Capture Templates"
  :group 'org)

(defcustom doct-default-entry-type 'entry
  "The default template entry type.
Can be overridden by using the :type keyword in a declarative form."
  :type '(choice (const :tag "Regular entry" entry)
                 (const :tag "plain list item" item)
                 (const :tag "checklist item" checkitem)
                 (const :tag "plain text" plain))
  :group 'doct)

(defcustom doct-option-merge-function 'doct--generic-merge
  "The function used to merge unrecognized option values.
It should take a single list of values and return a string."
  :type 'function
  :group 'doct)

(defcustom doct-merge-string-separator nil
  "The separator for joining a list of strings passed to `doct--generic-merge'."
  :type 'string
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

(defvar doct-template-keywords '(:template :template-file)
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
(define-error 'doct-no-target "Form has no target" 'doct-error)
(define-error 'doct-no-template "Form has no template" 'doct-error)

(defun doct--additive-keyword-p (keyword)
  "Return t if keyword starts with a +."
  (and (keywordp keyword)
       (string-prefix-p ":+" (symbol-name keyword))))

(defun doct--additive-keyword (keyword)
  "Return the additive version of KEYWORD."
  (unless (keywordp keyword)
    (signal 'doct-wrong-type-argument `(keyword-p ,keyword)))
  (if (doct--additive-keyword-p keyword)
      keyword
    (intern (replace-regexp-in-string "^:" ":+" (symbol-name keyword)))))

(defun doct--normalize-keyword (keyword)
  "Returns a plain keyword if given an additive keyword."
  (unless (keywordp keyword)
    (signal 'doct-wrong-type-argument `(keyword-p ,keyword)))
  (if (doct--additive-keyword-p keyword)
      (intern (concat ":" (substring (symbol-name keyword) 2)))
    keyword))

(defun doct-get (keyword)
  "Return KEYWORD's value from doct-options in `org-capture-plist'.
Intended to be used at capture template time."
  (plist-get (plist-get org-capture-plist :doct-options) keyword))

(defun doct--replace-template-strings (string)
  "Replace each occurrence of %doct(KEYWORD) with it's corresponding doct-options value."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward "%doct(\\(.*?\\))" nil :no-error)
        (replace-match (doct-get (intern (concat ":" (match-string 1)))))))
    (buffer-string)))

(defun doct--expansion-syntax-p (string)
  "Return t if STRING contains %doct(keyword) syntax,
else nil."
  (when (string-match-p "%doct(.*?)" string) t))

(defmacro doct--maybe-expand-template-string (template)
  "If TEMPLATE contains %doct:option expansion syntax, return a lambda
that can be executed at runtime. Otherwise, just return TEMPLATE."
  (if (doct--expansion-syntax-p template)
      `(lambda ()
         (doct--replace-template-strings ,template))
    template))

(defun doct--fill-deferred-template (string)
  "Call lambda expanded by `doct--maybe-expand-template-string' at capture time."
  (if (doct--expansion-syntax-p string)
      (funcall (macroexpand-1 `(doct--maybe-expand-template-string ,string)))
    string))

(defmacro doct--defer-merge (keyword values)
  "Return a lambda that can be called at runtime by `org-capture'."
  ;;functions, symbols that eval to strings, strings...etc
  (pcase keyword
    ((or :function (pred (lambda (keyword) (member keyword doct-hook-keywords))))
     `(lambda () (mapc 'funcall ',values)))
    (:file `(lambda ()
              (let ((merged (string-join (mapcar
                                          (lambda (val)
                                            (cond ((stringp val) val)
                                                  ((functionp val) (funcall val))
                                                  ((boundp val)
                                                   (symbol-value val))))
                                          ',values) "")))
                (if (string= "" merged)
                    org-default-notes-file
                  merged))))
    (:template `(lambda ()
                  (string-join
                   (mapcar
                    (lambda (val)
                      (cond ((stringp val)
                             (doct--fill-deferred-template val))
                            ((and (listp val) (seq-every-p 'stringp val))
                             (string-join
                              (mapcar (lambda (string)
                                        (doct--fill-deferred-template string))
                                      val) "\n"))
                            ((functionp val)
                             (doct--fill-deferred-template (funcall val)))
                            ((boundp val) (symbol-value val))))
                    ',values) "")))))

(defun doct--custom-merge-function (keyword)
  "If a function matching the form doct-mergeKEYWORD exists return it.
Otherwise, return nil."
  (let ((merge-fn (intern (concat "doct-merge" (symbol-name keyword)))))
    (when (fboundp merge-fn)
      merge-fn)))

(defun doct--generic-merge (values)
  (cond
   ;;single items gets returned
   ;;if it is a number, cast to string
   ((= (length values) 1)
    (let ((val (car values)))
      (cond
       ((stringp val) val)
       ((numberp val) (number-to-string val))
       (t val))))
   ;;if the whole list is strings, join them
   ((seq-every-p 'stringp values)
    (string-join values doct-merge-string-separator))
   ;;if the whole list is numbers, add them
   ((seq-every-p 'numberp values)
    (number-to-string (apply '+ values)))
   ;;These cases should be handled by user with a custom merge function.
   (t values)))

(defun doct--merge (values keyword)
  "Merge a list of KEYWORD's VALUES into a single type."
  ;;return nil if values empty
  (when values
    (pcase keyword
      ('nil nil)
      ((or :file :template)
       (when (eq keyword :file)
         (dolist (val values)
           (doct--validate-file val)))
       ;;skip deferral for static strings
       (let ((flattened (flatten-list values)))
         (if (and (seq-every-p 'stringp flattened)
                  (not (seq-some 'doct--expansion-syntax-p flattened)))
             (string-join (mapcar (lambda (val) (if (listp val)
                                                    (string-join val "\n") val))
                                  values) "")
           ;;Could be list of strings or a string
           (macroexpand-1 `(doct--defer-merge ,keyword ,values)))))
      ((or :function (pred (lambda (keyword) (member keyword doct-hook-keywords))))
       ;;skip deferral for single function
       (if (= (length values) 1)
           (car values)
         (macroexpand-1 `(doct--defer-merge :function, values))))
      ;;Strings or a list of strings
      (:olp (flatten-list values))
      ;;These keywords should only be declared once
      ((or :type :id) (car values))
      ;;These keywords can only be strings
      ((or :headline :keys :regexp :template-file)
       (unless (seq-every-p 'stringp values)
         (signal 'doct-wrong-type-argument `((stringp) ,values
                                             ,doct--current-form)))
       (mapconcat 'identity values ""))
      ;;these are all single values
      ((pred (lambda (keyword) (member keyword doct-option-keywords)))
       (car values))
      (_ (funcall (or (doct--custom-merge-function keyword)
                      doct-option-merge-function)
                  values)))))

(defun doct--get (form keyword &optional pair)
  "Recursively search FORM and FORM's ancestors for KEYWORD.
Returns KEYWORD's value.
If PAIR is non-nil, return a (KEY VAL) list."
  (let ((additive (doct--additive-keyword keyword))
        values)
    (letrec ((recurse (lambda (form)
                        (let ((member (plist-member form keyword))
                              (additive-val (plist-get form additive)))
                          (if member
                              (push (cadr member) values)
                            (when additive-val
                              (push additive-val values))
                            (when-let ((parent (plist-get form :doct--parent)))
                              (funcall recurse parent)))))))
      (funcall recurse form)
      (if pair
          `(,keyword ,(doct--merge values keyword))
        (doct--merge values keyword)))))

(defun doct--first-in-form (form keywords)
  "Find first occurence of one of KEYWORDS in FORM.
If not found in FORM, recursively search FORM's ancestors.
Return (KEYWORD VAL)."
  (let ((origin form))
    (letrec ((recurse
              (lambda (form)
                (if-let ((keyword (car
                                   (seq-some (lambda (element)
                                               (and (keywordp element)
                                                    (member element keywords)))
                                             form))))
                    (doct--get origin keyword t)
                  (when-let ((parent (plist-get form :doct--parent)))
                    (funcall recurse parent))))))
      (funcall recurse form))))

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
                         ('nil (mapcar #'intern-soft
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
    (user-error "DOCT Could not add %s as doct--hook/%s to %s" fn keys where)))

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

(defun doct--validate-file (target)
  "Check to see if TARGET is a valid :file target. If it is, return TARGET.
Otherwise, throw an error."
  (unless (or (stringp target)
              (functionp target)
              (and (symbolp target) (not (or (eq t target) (keywordp target)))))
    (signal 'doct-wrong-type-argument
            `(stringp functionp symbolp ,target ,doct--current-form))))

(defun doct--target-file (form file-target)
  "Convert declarative FORM's :file and file-extensions to Org capture template syntax.
FILE is the value for FORM's :file keyword."
  (doct--validate-file file-target)
  (let (type target)
    (pcase (doct--first-in-form form
                                ;;datetree is only used when :olp is specified
                                (remq :datetree doct-file-extension-keywords))
      (`(:olp ,path) (unless (and (listp path)
                                  (seq-every-p 'stringp path))
                       (signal 'doct-wrong-type-argument
                               `((listp stringp) ,path ,doct--current-form)))
       (when (doct--get form :datetree)
         (push :datetree type))
       (push :olp type)
       (dolist (heading (nreverse path))
         (push heading target)))
      ;;function headline regexp
      (`(,keyword ,extension)
       (let ((predicate (if (eq keyword :function) 'functionp 'stringp)))
         (unless (funcall predicate extension)
           (signal 'doct-wrong-type-argument
                   `(,predicate ,extension ,doct--current-form)))
         (push extension target)
         (push keyword type))))
    (push :file type)
    (push file-target target)
    `(,(intern (string-join
                (mapcar (lambda (keyword)
                          (substring (symbol-name keyword) 1))
                        (delq nil type)) "+"))
      ,@(delq nil target))))

(defun doct--target (form)
  "Convert declarative FORM's target to Org captures target."
  (pcase (doct--first-in-form form doct-exclusive-location-keywords)
    ((and (or 'nil `(,key nil)) nil-target)
     (signal 'doct-no-target `(,doct-exclusive-location-keywords
                               ,nil-target
                               ,doct--current-form)))
    (`(:clock ,bool) '(clock))
    (`(:id ,id) `(id ,id))
    (`(:function ,fn) (unless (doct--get form :file)
                        `(function ,fn)))
    (`(:file ,file) (doct--target-file form file))))

(defun doct--template (form)
  "Convert FORM's template target to Org capture template syntax."
  (pcase (doct--first-in-form form doct-template-keywords)
    (`(:template-file ,file) `(file ,file))
    (`(:template ,template)
     (pcase template
       ((or 'nil (and (pred stringp) (pred string-empty-p))) nil)
       ((pred functionp) `(function ,template))
       ((pred stringp) template)
       ((and (pred listp) (guard (seq-every-p 'stringp template)))
        (string-join template "\n"))
       (_ (signal 'doct-wrong-type-argument
                  `((stringp listp functionp) ,template ,doct--current-form)))))
    (_ nil)))

(defun doct--additional-properties (form)
  "Convert FORM's additional properties to Org capture syntax.
Returns a list of ((ADDITIONAL OPTIONS) (CUSTOM PROPERTIES))."
  (letrec ((recurse
            (lambda (form)
              (let ((keywords (delete-dups (seq-filter 'keywordp form))))
                (dolist (keyword keywords)
                  (let ((keyword (doct--normalize-keyword keyword)))
                    (cond
                     ((and (not (member keyword additional-options))
                           (member keyword doct-option-keywords))
                      (push keyword additional-options)
                      (push (doct--get form keyword) additional-options))
                     ((not (or (member keyword custom-properties)
                               (member keyword doct-recognized-keywords)))
                      (push keyword custom-properties)
                      (push (doct--get form keyword) custom-properties)))))
                (when-let ((parent (plist-get form :doct--parent)))
                  (funcall recurse parent)))))
           (additional-options nil)
           (custom-properties nil))
    (funcall recurse form)
    `(,(nreverse additional-options) ,(nreverse custom-properties))))

(defun doct--add-hooks (name form keys)
  "Add hooks declared in FORM for template NAME with KEYS."
  (dolist (keyword doct-hook-keywords)
    (when-let ((hook-fn (doct--get form keyword))
               (hook (if (eq keyword :hook) "mode"
                       ;;remove preceding ':' from keyword
                       (substring (symbol-name keyword) 1))))
      (unless (functionp hook-fn)
        (signal 'doct-wrong-type-argument `(functionp ,hook-fn
                                                      ,doct--current-form)))
      (doct--add-hook keys hook-fn hook name))))

(defun doct--type (form)
  "Return FORM's :type value or `doct-default-entry-type'."
  (let ((type (or (doct--get form :type)
                  doct-default-entry-type))
        (entry-types '(entry item checkitem table-line plain)))
    (unless (member type entry-types)
      (signal 'doct-wrong-type-argument `(,entry-types ,type)))
    type))

(defun doct--convert (name &rest properties)
  "Convert declarative form to template named NAME with PROPERTIES.
For a full description of the PROPERTIES plist see `doct'."
  (setq doct--current-form `(,name ,@properties))

  (unless (or (stringp name) (symbolp name))
    (signal 'doct-wrong-type-argument
            `((stringp symbolp) ,name ,doct--current-form)))

  (let* ((children (plist-get properties :children))
         (symbolic-parent (symbolp name))
         (keys (unless symbolic-parent (doct--keys properties)))
         entry)

    (when children
      (setq children (mapcar (lambda (child)
                               (apply #'doct--convert
                                      (append child
                                              `(:doct--parent ,properties))))
                             ;;allow a single child to be declared without
                             ;;manually nesting it in a list
                             (if (not (seq-every-p 'listp children))
                                 `(,children)
                               children))))

    (unless children
      (doct--add-hooks name properties keys))

    (setq entry `(,keys
                  ,name
                  ,@(unless children
                      `(,(doct--type properties)
                        ,(doct--target properties)
                        ,(doct--template properties)
                        ,@(when-let ((additional-properties
                                      (doct--additional-properties properties)))
                            `(,@(car additional-properties)
                              ,@(when-let
                                    ((custom-opts (cadr additional-properties)))
                                  `(:doct-options ,custom-opts))))))))

    (if children
        (if (symbolp name)
            `(,@children)
          `(,entry ,@children))
      entry)))

(defun doct-flatten-lists-in (list-of-lists)
  "Flatten each list in LIST-OF-LISTS.
For example:
  '((1) ((2 3) (4)) (((5))))
returns:
  '((1) (2) (3) (4) (5))"
  (let (flattend)
    (letrec ((flatten (lambda (list)
                        (dolist (element list)
                          (if (seq-every-p 'listp element)
                              (funcall flatten element)
                            (push element flattend))))))
      (funcall flatten list-of-lists)
      (nreverse flattend))))

;;@INCOMPLETE needs overview docstring
(defun doct--maybe-convert-form (form)
  (condition-case err
      (apply 'doct--convert form)
    (doct-error (user-error "DOCT %s" (error-message-string err)))))

(defun doct (declarations)
  "DECLARATIONS is a list of declarative forms."
  (let* ((entries (mapcar 'doct--maybe-convert-form declarations)))
    (unwind-protect
        (progn
          (run-hook-with-args 'doct-after-conversion-hook entries)
          ;;hook functions may set doct-templates to return manipulated list
          (or doct-templates (doct-flatten-lists-in entries)))
      (setq doct-templates nil))))

(provide 'doct)

;;; doct.el ends here
