;;; doct.el --- DOCT: Declarative Org capture templates -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Nicholas Vollmer

;; Author: Nicholas Vollmer <progfolio@protonmail.com>
;; URL: https://github.com/progfolio/doct
;; Created: December 10, 2019
;; Keywords: org, convenience
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0

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
;; templates. See the doct docstring for more details.

;;; Code:
(require 'subr-x)
(require 'seq)
(require 'org-capture)


(defgroup doct nil
  "DOCT: Declarative Org Capture Templates"
  :group 'org)

(defcustom doct-default-entry-type 'entry
  "The default template entry type.
It can be overridden by using the :type keyword in a declarative form."
  :type '(choice (const :tag "Regular entry" entry)
                 (const :tag "plain list item" item)
                 (const :tag "checklist item" checkitem)
                 (const :tag "plain text" plain))
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
Its value is not stored between invocations to doct.")

(defvar doct--current nil
  "The current form being processed by doct. Used for error processing.")

(defvar doct-entry-types '(entry item checkitem table-line plain)
  "The allowed template entry types.")

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

(defvar doct-context-keywords '(:in-buffer
                                :in-file
                                :in-mode
                                :unless-buffer
                                :unless-file
                                :unless-mode
                                :function
                                :when
                                :unless)
  "Keywords that define a templates contexts.")

(defvar doct-recognized-keywords `(:children
                                   :contexts
                                   :custom
                                   :disabled
                                   :doct-keys
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

;;doct-error is just parent error symbol.
;;Not intended to be directly signaled.
(define-error 'doct-error               "DOCT peculiar error!")
(define-error 'doct-no-keys             "Form has no :keys value" 'doct-error)
(define-error 'doct-group-keys          "Group has :keys value"   'doct-error)
(define-error 'doct-no-target           "Form has no target"      'doct-error)
(define-error 'doct-no-template         "Form has no template"    'doct-error)
(define-error 'doct-wrong-type-argument "Wrong type argument"     'doct-error)

;;;###autoload
(defun doct-get (keyword)
  "Return KEYWORD's value from doct-custom in `org-capture-plist'.
Intended to be used at capture template time."
  (plist-get (plist-get org-capture-plist :doct-custom) keyword))

(defun doct--replace-template-strings (string)
  "Replace STRING's %doct(KEYWORD) occurrences with their doct-custom values."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward "%doct(\\(.*?\\))" nil :no-error)
        (replace-match (or (doct-get (intern (concat ":" (match-string 1))))
                           ""))))
    (buffer-string)))

(defun doct--expansion-syntax-p (string)
  "Return t for STRING containing %doct(keyword) syntax, else nil."
  (when (string-match-p "%doct(.*?)" string) t))

(defun doct--fill-template (val)
  "Fill VAL."
  (cond
   ((stringp val)
    (if (doct--expansion-syntax-p val)
        (doct--replace-template-strings val)
      val))
   ((functionp val)
    (doct--fill-template (funcall val)))
   ((listp val)
    (unless (seq-every-p #'stringp val)
      (signal 'doct-wrong-type-argument `((stringp) ,val ,doct--current)))
    (string-join
     (if (seq-some #'doct--expansion-syntax-p val)
         (mapcar #'doct--fill-template val)
       val) "\n"))
   (t (signal 'doct-wrong-type-argument `((stringp listp functionp) ,val ,doct--current)))))

(defun doct--first-in (plist keywords)
  "Find first non-nil occurrence of one of KEYWORDS in PLIST.
If not found in PLIST, recursively search FORM's ancestors.
Return (KEYWORD VAL)."
  (seq-some (lambda (keyword)
              (let ((val (plist-get plist keyword)))
                (when (and val (member keyword keywords))
                  `(,keyword ,val))))
            (seq-filter #'keywordp plist)))

(defun doct--validate-file (target)
  "Type check :file TARGET."
  (unless (or (stringp target)
              (functionp target)
              (and (symbolp target) (not (or (eq t target) (keywordp target)))))
    (signal 'doct-wrong-type-argument
            `(stringp functionp symbolp ,target ,doct--current))))

(defun doct--target-file (plist file-target)
  "Convert PLIST's :file and file-extensions to Org capture template syntax.
FILE-TARGET is the value for PLIST's :file keyword."
  (doct--validate-file file-target)
  (let (type target)
    ;;datetree is only used when :olp is specified
    (pcase (doct--first-in plist (remq :datetree doct-file-extension-keywords))
      (`(:olp ,path) (unless (and (listp path) (seq-every-p #'stringp path))
                       (signal 'doct-wrong-type-argument
                               `((listp stringp) ,path ,doct--current)))
       (when (plist-get plist :datetree)
         (push :datetree type))
       (push :olp type)
       (dolist (heading (nreverse (seq-copy path)))
         (push heading target)))
      ;;function headline regexp
      (`(,keyword ,extension)
       (when extension
         (let ((predicate (if (eq keyword :function) (lambda (val)
                                                       (or (functionp val)
                                                           (null val)))
                            #'stringp)))
           (unless (funcall predicate extension)
             (signal 'doct-wrong-type-argument
                     `(,predicate ,extension ,doct--current)))
           (push extension target)
           (push keyword type)))))
    (push :file type)
    (push file-target target)
    `(,(intern (string-join
                (mapcar (lambda (keyword)
                          (substring (symbol-name keyword) 1))
                        (delq nil type)) "+"))
      ,@(delq nil target))))

(defun doct--target (plist)
  "Convert PLIST's target to Org capture template target."
  (pcase (doct--first-in plist doct-exclusive-location-keywords)
    ((and (or 'nil `(,key nil)) nil-target)
     (signal 'doct-no-target `(,doct-exclusive-location-keywords
                               ,nil-target
                               ,doct--current)))
    (`(:clock ,_) '(clock))
    (`(:id ,id) (if (stringp id)
                    `(id ,id)
                  (signal 'doct-wrong-type-argument
                          '(stringp ,id ,doct--current))))
    (`(:function ,fn) (if (functionp fn)
                          (if-let ((file (plist-get plist :file)))
                              (doct--target-file plist file)
                            `(function ,fn))
                        (signal 'doct-wrong-type-argument
                                '(functionp ,fn ,doct--current))))
    (`(:file ,file) (doct--target-file plist file))))

(defun doct--template-filler (symbol val)
  "Generate function named SYMBOL to fill current :template VAL at capture time."
  (eval `(defun ,symbol ()
           ,(concat "Fill template \""
                    (car (last (split-string (symbol-name symbol) "/")))
                    "\" at capture time.")
           (doct--fill-template ',val))))

(defun doct--defer (plist val)
  "Return deferred filler function for PLIST with VAL partially applied."
  (pcase val
    ((or (pred functionp) (pred stringp) (pred listp))
     (let* ((keys (doct--keys plist))
            (fill-fn-symbol (intern (concat "doct--fill/" keys)))
            (fn (doct--template-filler fill-fn-symbol val)))
       `(function ,fn)))
    (_ (signal 'doct-wrong-type-argument
               `((stringp listp functionp) ,val ,doct--current)))))

(defun doct--template (plist)
  "Convert PLIST's template target to Org capture template syntax."
  (pcase (doct--first-in plist doct-template-keywords)
    (`(:template-file ,file) (if (stringp file)
                                 `(file ,file)
                               (signal 'doct-wrong-type-argument
                                       '(stringp ,file ,doct--current))))
    (`(:template ,template)
     ;;simple values: nil string, list of strings with no expansion syntax
     (pcase template
       ((or 'nil "") nil)
       ((and (pred stringp)
             (guard (not (doct--expansion-syntax-p template))))
        template)
       ((and (pred listp)
             (guard (seq-every-p #'stringp template))
             (guard (not (seq-some #'doct--expansion-syntax-p template))))
        (string-join template "\n"))
       (deferred (doct--defer plist deferred))))))

(defun doct-plist-p (list)
  "Non-null if and only if LIST is a plist with keyword keys."
  (while (consp list)
    (setq list (if (and (keywordp (car list))
                        (consp (cdr list)))
                   (cddr list)
                 'not-plist)))
  (null list))

(defun doct--custom (plist)
  "Type check and return PLIST's custom property."
  (when-let ((custom (plist-get plist :custom)))
    (unless (doct-plist-p custom)
      (signal 'doct-wrong-type-argument `(plist ,custom ,doct--current)))
    custom))

(defun doct--additional-properties (plist)
  "Convert PLIST's additional properties to Org capture syntax.
Returns a list of ((ADDITIONAL OPTIONS) (CUSTOM PROPERTIES))."
  (let ((keywords (delete-dups (seq-filter #'keywordp plist)))
        additional-options
        custom-properties)
    (dolist (keyword keywords)
      (cond
       ((and (not (member keyword additional-options))
             (member keyword doct-option-keywords))
        (push keyword additional-options)
        (push (plist-get plist keyword) additional-options))
       ((not (or (member keyword custom-properties)
                 (member keyword doct-recognized-keywords)))
        (push keyword custom-properties)
        (push (plist-get plist keyword) custom-properties))))
    `(,(nreverse additional-options)
      ,(let ((custom (nreverse custom-properties)))
         (if-let ((explicit (doct--custom plist)))
             `(,@explicit ,@custom)
           custom)))))

(defun doct--inherit (parent child)
  "Inherit PARENT's plist members unless CHILD has already declared them.
The only exceptions to this are the :keys and :children and :group properties.
CHILD's keys are prefixed with PARENT's.
The :children and :group properties are ignored."
  ;;remove :group description
  (when (stringp (car child))
    (pop child))
  (dolist (keyword (seq-filter (lambda (el)
                                 (and (keywordp el)
                                      (not (member el '(:children :group)))))
                               parent))
    (let ((keysp (member keyword '(:keys :doct-keys))))
      (unless (and (plist-member child keyword) (not keysp))
        (if keysp
            (plist-put child :doct-keys (concat (or (plist-get parent :doct-keys)
                                                    (plist-get parent :keys))
                                                (plist-get child :keys)))
          (plist-put child keyword (plist-get parent keyword))))))
  child)

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
                     ,(concat "It runs as part of `" (symbol-name hook) "'"
                              (if entry-name
                                  (concat " when selecting the \"" entry-name
                                          "\" template.")
                                "'."))
                     "It can be removed using `doct-remove-hooks' like so:"
                     ,(concat "(doct-remove-hooks \""
                              keys "\" \\='" where " t)"))
                   "\n")
                 (when (string= ,keys (plist-get org-capture-plist :key))
                   (funcall (quote ,fn)))))
        (add-hook hook wrapper))
    (user-error "DOCT Could not add %s as doct--hook/%s to %s" fn keys where)))

(defun doct--add-hooks (name plist keys)
  "Add hooks declared in PLIST for template NAME with KEYS."
  (dolist (keyword doct-hook-keywords)
    (when-let ((hook-fn (plist-get plist keyword))
               (hook (if (eq keyword :hook) "mode"
                       ;;remove preceding ':' from keyword
                       (substring (symbol-name keyword) 1))))
      (unless (functionp hook-fn)
        (signal 'doct-wrong-type-argument `(functionp ,hook-fn ,doct--current)))
      (doct--add-hook keys hook-fn hook name))))

(defun doct--entry-type (plist)
  "Return PLIST's :type value or `doct-default-entry-type'."
  (let ((type (or (plist-get plist :type) doct-default-entry-type)))
    (or (car (member type doct-entry-types))
        (signal 'doct-wrong-type-argument
                `(,doct-entry-types ,type ,doct--current)))))

(defun doct--keys (plist &optional group)
  "Type check and return PLIST's :keys value.
If GROUP is non-nil, make sure there is no :keys value."
  (let ((keys (plist-member plist :keys))
        (inherited (plist-member plist :doct-keys)))
    (when (and group keys)
      (signal 'doct-group-keys `(,doct--current)))
    (unless (or group keys inherited) (signal 'doct-no-keys `(,doct--current)))
    (let ((keys (or (plist-get plist :doct-keys) (plist-get plist :keys))))
      (unless (or (stringp keys) group)
        (signal 'doct-wrong-type-argument `(stringp ,keys ,doct--current)))
      keys)))

(defun doct--children (plist)
  "Type check and return PLIST's children property."
  (let ((children (plist-get plist :children)))
    (if (and (listp children) (not (functionp children)))
        children
      (signal 'doct-wrong-type-argument `(listp ,children ,doct--current)))))

(defun doct--compose-entry (keys name properties parent)
  "Return a template suitable for `org-capture-templates'.
The list is of the form: (KEYS NAME type target template additional-options...).
PROPERTIES provides the type, target template and additional options.
If PARENT is non-nil, list is of the form (KEYS NAME)."
  `(,keys
    ,name
    ,@(unless parent
        `(,(doct--entry-type properties)
          ,(doct--target properties)
          ,(doct--template properties)
          ,@(when-let ((additional-properties
                        (doct--additional-properties properties)))
              `(,@(car additional-properties)
                ,@(when-let ((custom-opts (cadr additional-properties)))
                    `(:doct-custom ,custom-opts))))))))

(defun doct--convert-constraint-keyword (keyword)
  "Convert KEYWORD to `org-capture-templates-contexts' equivalent symbol."
  (let ((name (symbol-name keyword)))
    (intern (if (string-prefix-p ":unless" name)
                (replace-regexp-in-string "^:unless" "not-in" name)
              (replace-regexp-in-string "^:" "" name)))))

(defmacro doct--constraint-function (constraint value)
  "CONSTRAINT is a context keyword. VALUE is its value in the current rule."
  (let* ((name (symbol-name constraint))
         (test `(string-match val ,(cond
                                    ((string-suffix-p "buffer" name) '(buffer-name))
                                    ((string-suffix-p "file" name) '(or (buffer-file-name) ""))
                                    ((string-suffix-p "mode" name) '(symbol-name major-mode)))))
         (fn `(seq-some (lambda (val) ,test) ',value)))
    (if (string-prefix-p ":unless" name)
        `(lambda ()
           (not ,fn))
      `(lambda () ,fn))))

(defmacro doct--conditional-constraint (condition value)
  "Return a lambda which wraps VALUE in the appropraite CONDITION form.
CONDITION is either when or unless."
  (let ((condition-form (if (functionp value)
                            `(,value)
                          value)))
    `(lambda () (,condition ,condition-form t))))

(defun doct--constraint-rule-list (constraint value)
  "Create a rule list for CONSTRAINT with VALUE."
  `(,(cond
      ((eq constraint :function) (if (functionp value)
                                     value
                                   (signal 'doct-wrong-type-argument
                                           `(functionp ,value ,doct--current))))
      ((eq constraint :when)   (doct--conditional-constraint when   value))
      ((eq constraint :unless) (doct--conditional-constraint unless value))
      ((stringp value)
       `(,(doct--convert-constraint-keyword constraint)
         . ,value))
      ((and (listp value) (seq-every-p #'stringp value))
       (macroexpand
        `(doct--constraint-function ,constraint ,value)))
      (t (signal 'doct-wrong-type-argument
                 `(stringp listp ,value ,doct--current))))))

(defun doct--add-contexts (properties)
  "Set up `org-capture-template-contexts' for PROPERTIES."
  (when-let ((contexts (plist-get properties :contexts)))
    (let ((template-keys (doct--keys properties))
          rules)
      ;;allow a single context rule or a list of context rules
      (dolist (context (if (seq-every-p #'listp contexts) contexts `(,contexts)))
        (if-let ((first-keyword (doct--first-in context doct-context-keywords)))
            (let* ((constraint (car first-keyword))
                   (value (cadr first-keyword))
                   (context-keys (plist-get context :keys))
                   (rule-list (doct--constraint-rule-list constraint value))
                   (rule (delq nil `(,template-keys ,context-keys ,rule-list))))
              (push rule rules))
          (signal 'doct-wrong-type-argument `(,@doct-context-keywords nil ,doct--current))))
      (dolist (rule (nreverse rules))
        (add-to-list 'org-capture-templates-contexts rule)))))

(defun doct--convert (name &rest properties)
  "Convert declarative form to a template named NAME with PROPERTIES.
For a full description of the PROPERTIES plist see `doct'."
  (unless (eq (plist-get properties :disabled) t)
    (setq doct--current `(,name ,@properties))
    (let ((group (eq name :group)))
      (unless (or (stringp name) group)
        (signal 'doct-wrong-type-argument
                `((stringp symbolp) ,name ,doct--current)))
      (when group
        ;;remove :group description
        (if (stringp (car properties))
            (setq properties (cdr properties))))
      (let* ((children (doct--children properties))
             (keys (doct--keys properties group))
             entry)
        (when children
          (setq children (mapcar (lambda (child)
                                   (apply #'doct--convert
                                          `(,(car child)
                                            ,@(doct--inherit properties
                                                             (cdr child)))))
                                 (if (seq-every-p #'listp children)
                                     children
                                   `(,children)))))
        (unless children
          (doct--add-hooks name properties keys)
          (doct--add-contexts properties))
        (unless group
          (setq entry (doct--compose-entry keys name properties children)))
        (if children
            (if group
                `(,@children)
              `(,entry ,@children))
          entry)))))

;;;###autoload
(defun doct-flatten-lists-in (list-of-lists)
  "Flatten each list in LIST-OF-LISTS.
For example:
  '((1) ((2 3) (4)) (((5))))
returns:
  '((1) (2) (3) (4) (5))"
  (let (flattend)
    (letrec ((flatten (lambda (list)
                        (dolist (element list)
                          (if (seq-every-p #'listp element)
                              (funcall flatten element)
                            (push element flattend))))))
      (funcall flatten list-of-lists)
      (nreverse flattend))))

(defun doct--maybe-convert-form (form)
  "Attempt to convert FORM to Org capture template syntax."
  (condition-case-unless-debug err
      (apply #'doct--convert form)
    (doct-error (user-error "DOCT %s" (error-message-string err)))))

;;;###autoload
(defun doct (declarations)
  "DECLARATIONS is a list of declarative forms.
Each declaration is either a child, parent, or group.

A child declaration must have:

  - a name
  - a :keys string
  - a template type
  - a target
  - a template

and may also have:

  - hook functions defined with the hook keywords
  - additional arguments

A parent declaration must have:

  - a name
  - a :keys string
  - a list of :children

and may also have additional properties inherited by its children.

A group is a special kind of parent declaration.
Its children inherit its properties.
It is not added to the template selection menu.
Its name must be the :group keyword.
It may optionally have a descriptive string for the value of :group.
It must not have a :keys value.

  (doct \\='((\"Work\" :keys \"w\" :file \"~/org/work.org\" :children
         ((:group \"Clocked\" :clock-in t :children
                  ((\"Call\" :keys \"p\" :template \"* Phone call with %?\")
                   (\"Meeting\" :keys \"m\" :template \"* Meeting with %?\")))
          (\"Browsing\" :keys \"b\" :template \"* Browsing %x\")))))

Returns:

  ((\"w\" \"Work\")
   (\"wp\" \"Call\" entry
    (file \"~/org/work.org\") \"* Phone call with %?\" :clock-in t)
   (\"wm\" \"Meeting\"    entry
    (file \"~/org/work.org\") \"* Meeting with %?\"    :clock-in t)
   (\"wb\" \"Browsing\"   entry (file \"~/org/work.org\") \"* Browsing %x\"))

Inherited Properties
====================

A child inherits its ancestors' properties.
It may optionally override an inherited property by specifying that property \
directly.
For example, considering:

  (doct \\='((\"Grandparent\" :keys \"g\"
              :file \"example.org\"
              :children (\"Parent\" :keys \"p\"
                         :children (\"Child\" :keys \"c\")))))

The \"Child\" template inherits its :file property from the \"Grandparent\" \
declaration.
The \"Parent\" declaration could override this value:

  (doct \\='((\"Grandparent\" :keys \"g\"
              :file \"example.org\"
              :children (\"Parent\" :keys \"p\"
                         :file \"overridden.org\"
                         :children (\"Child\" :keys \"c\")))))

And the \"Child\" would have its :file property set to \"overridden.org\".

Name & Keys
===========

Every declaration must define a name.
Unless it is a group, it must also define a :keys value.
The name is the first value in the declaration.
The :keys keyword defines the keys to access the template from the capture menu.

  (doct \\='((\"example\" :keys \"e\"...)))

  returns:

  ((\"e\" \"example\"...))

Type
====

The :type keyword defines the template's entry type and accepts the following \
symbols:

  - entry
    An Org node with a headline.
    The template becomes a child of the target entry or a top level entry.

  - item
    A plain list item, placed in the first plain list at the target location.

  - checkitem
    A checkbox item.
    Same as plain list item only it uses a different default template.

  - table-line
    A new line in the first table at target location.

  - plain
    Text inserted as is.

`doct-default-entry-type' defines the entry type when the :type keyword is not \
provided.

For example, with `doct-default-entry-type' set to entry (the default):

  (doct \\='((\"example\"
              :keys \"e\"
              :type entry
              :file \"\")))

  and:

  (doct \\='((\"example\"
              :keys \"e\"
              :file \"\")))

  both return:

  ((\"e\" \"example\" entry (file \"\") nil))

Target
======

The target defines the location of the inserted template text.

The first keyword declared in the following group exclusively sets the target.
The :file keyword is not necessary for these.

  - :id \"id of existing Org entry\"
    File as child of this entry, or in the body of the entry
    (see `org-id-get-create')

  - :clock t
    File to the currently clocked entry

  - :function (lambda () ;visit file and move point to desired location...)
    This keyword is exclusive when used without the :file keyword.
    It is responsible for finding the proper file and location to insert the \
capture item.
    If :file defines a target file, then the function is only responsible for \
moving point to the desired location within that file.

  (doct \\='((\"example\"
              :keys \"e\"
              :type entry
              :clock t
              ;;ignored because :clock is first
              :function (lambda () (ignore))
              ;;also ignored
              :id \"1\")))

  returns:

  ((\"e\" \"example\" entry (clock) nil))

The :file keyword defines the target file for the capture template.

  (doct ... :file \"/path/to/target.org\")

The following keywords refine the target file location:

  - :headline \"node headline\"
    File under unique heading in target file.

  - :olp (\"Level 1 heading\" \"Level 2 heading\"...)
    Define the full outline in the target file.
    If :datetree has a non-nil value, create a date tree for today's date.
    Use a non-nil :time-prompt property to prompt for a different date.
    Set the :tree-type property to the symbol 'week' to make a week tree \
instead of the default month tree.

  - :regexp \"regexp describing location\"
    File to entry matching regexp in target file

  - :function location-finding-function
    If used in addition to the :file keyword, the value should be a function \
that finds the desired location in that file.
    If used as an exclusive keyword (see above), the function must locate \
both the target file and move point to the desired location.

Template
========

The :template keyword defines the template for creating the capture item.
It may be either a string, list of strings, or a function.
doct joins the list with new lines.
A function must return the template text.

  (doct \\='((... :template (\"Test\" \"One\" \"Two\"))))

  returns:

  ((... \"Test\\nOne\\nTwo\"))

The :template-file keyword defines a file containing the text of the template.

The first keywords declared overrides any additional template declarations.

Additional Options
==================

Key-value pairs define additional options.

  (doct \\='((... :immediate-finish t)))

  returns:

  ((... :immediate-finish t))

see `org-capture-templates' for a full list of additional options.

Custom data
===========

doct stores unrecognized keywords on the template's `org-capture-plist' \
as members of the doct-custom plist.
This makes a template's metadata accessible during capture.
See \"Doct String Expansion\" below for detail on using that data.

The :custom keyword accepts a plist.
The doct-custom plist stores its elements.
This is only necessary if you wish to use a keyword which doct already uses.
For example:
  (doct \\='((\"Music Gear\" :keys \"m\" :file \"\"
           :custom (:keys \"Moog\")))

returns:

  (\"m\" \"Music Gear\" entry (file \"\") nil :doct-custom (:keys \"Moog\"))

Children
========

The :children keyword defines a parent's children.
Its value may be a single declaration or a list of declarations.
The parent's :keys prefix each child's :keys.

  (doct \\='((\"parent\" :keys \"p\"
              :children
              ((\"child\" :keys \"c\"
                :children
                ((\"grandchild\" :keys \"g\"
                  :file \"\"
                  :type plain
                  :template \"test\")))))))

  returns:

  ((\"p\" \"parent\")
   (\"pc\" \"child\")
   (\"pcg\" \"grandchild\" plain (file \"\") \"test\"))

%doct String Expansion
======================

A declaration may include custom metadata which is accessible during capture.
The syntax is similar to other, built-in \"%-escapes\":

  %doct(KEYWORD)

will insert the value declared with :KEYWORD in the template.
For example, with:

  (doct \\='((\"Parent\" :keys \"p\"
           :file \"\"
           :template \"* %doct(todo-state) %?\"
           :children ((\"One\" :keys \"1\" :todo-state \"TODO\")
                      (\"Two\" :keys \"2\" :todo-state \"IDEA\")))))

Each child template has its :todo-state value expanded in the inherited \
:template.

Hooks
=====

Adding one of the following hook keywords in a declaration will generate a \
function of the form:

  doct--hook/<hook-variable-abbreviation>/KEYS

which wraps the user's function in a conditional check for the current \
template's keys and adds it to the appropriate hook.

  - :hook
    `org-capture-mode-hook'
  - :prepare-finalize
    `org-capture-prepare-finalize-hook'
  - :before-finalize
    `org-capture-before-finalize-hook'
  - :after-finalize
    `org-capture-after-finalize-hook'

For example:

  (doct \\='((\"example\"
           :keys \"e\"
           :file \"\"
           :hook (lambda ()
                   ;;when selecting the \"example\" template
                   ;;doct--hook/mode/e executes
                   ;;during the org-capture-mode-hook.
                   (ignore)))))

defines the function doct--hook/mode/e:

    (defun doct--hook/mode/e ()
      \"Auto generated by `doct--add-hook'.
    It runs as part of `org-capture-mode-hook' when selecting the \"example\" \
template.
    It can be removed using `doct-remove-hooks' like so:
    (doct-remove-hooks \"e\" \\='mode t)\"
      (when (string= \"e\" (plist-get org-capture-plist :key))
        (funcall \\='(lambda nil (ignore)))))

and adds it to the `org-capture-mode-hook'.
See `doct-remove-hooks' to remove and unintern generated functions.

Contexts
========

The :contexts keyword defines contextual rules for a template.
Its value may be a single contextual rule or a list of rules.
The following keywords are available to create contextual rules:

  - :in-buffer regexp
    Show template when REGEXP matches the current buffer's name.

    (doct \\='((\"Only in *scratch*\" :keys \"n\" :file \"\"
             :contexts ((:in-buffer \"^*scratch*$\")))))

  - :unless-buffer regexp
    Show template unless REGEXP matches the current buffer's name.

    (doct \\='((\"Except in *scratch*\" :keys \"n\" :file \"\"
             :contexts ((:unless-buffer \"^*scratch*$\")))))

  - :in-file regexp
    Show template when REGEXP matches the current buffer's file name.

    (doct \\='((\"Only in work.org\" :keys \"n\" :file \"\"
             :contexts ((:in-file \"work\\.org$\")))))

  - :unless-file regexp
    Show template unless REGEXP matches the current buffer's file name.

    (doct \\='((\"Except in work.org\" :keys \"n\" :file \"\"
             :contexts ((:unless-file \"work\\.org$\")))))

  - :in-mode regexp
    Show template when REGEXP matches the current buffer's major mode.

    (doct \\='((\"Only in org-mode\" :keys \"n\" :file \"\"
             :contexts ((:in-mode \"org-mode\")))))

  - :unless-mode regexp
    Show template unless REGEXP matches the current buffer's major mode.

    (doct \\='((\"Except in org-mode\" :keys \"n\" :file \"\"
             :contexts ((:unless-mode \"org-mode\")))))

  - :when condition
    Show template when condition evaluates to a non-nil value.
    Condition may be a function or a single form.

    (doct \\='((\"Show when my/predicate-p returns t\" :keys \"n\" :file \"\"
             :contexts ((:when my/predicate-p)))))

    (doct \\='((\"1/3 chance of showing\" :keys \"n\" :file \"\"
             :contexts ((:when (= 2 (random 3)))))))

  - :unless condition
    Show template when condition evaluates to a nil value.
    Condition may be a function or a single form.

    (doct \\='((\"Show when my/predicate-p returns nil\" :keys \"n\" :file \"\"
             :contexts ((:unless my/predicate-p)))))

    (doct \\='((\"2/3 chance of showing\" :keys \"n\" :file \"\"
             :contexts ((:unless (= 2 (random 3)))))))

  - :function
    Show template when function returns non-nil.
    The function is not passed any arguments.

    (doct \\='((\"Between 9AM and 5PM\" :keys \"n\" :file \"\"
             :contexts
             ((:function (lambda ()
                           (<= 9
                               (string-to-number (format-time-string \"%H\"))
                               17)))))))

Adding :keys to a rule does the same as above, but remaps the template's keys \
to the template with keys matching the :keys string.
For example:

  (doct \\='((\"In *scratch* remapped to t, else use original template\"
           :keys \"n\" :file \"\"
           :contexts ((:unless-buffer \"^\\*scratch\\*$\" :keys \"n\")
                      (:in-buffer     \"^\\*scratch\\*$\" :keys \"t\")))))

Rule keywords, spare :function, :when, and :unless may also take a list of \
strings for their values.

  (doct \\='((\"Only in org-mode or emacs-lisp-mode\" :keys \"n\" :file \"\"
           :contexts ((:in-mode (\"org-mode\" \"emacs-lisp-mode\"))))))

Disabling Templates
===================

Setting the :disabled keyword to t disables a template.
The template's declaration is not error checked.
This can be useful if you don't have the time to deal with an error right away.
For example:

  (doct \\='((:group \"All\" :file \"\" :children
                  ((:group \"Enabled\" :children
                           ((\"One\"   :keys \"1\")
                            (\"Two\"   :keys \"2\")
                            (\"Three\" :keys \"3\")))
                   (:group \"Disabled\" :disabled t :children
                           ((\"Four\" :keys 4)
                            (\"Five\" :keys 5)
                            (\"Six\"  :keys 6)))))))

returns:

  ((\"1\" \"One\"   entry (file \"\") nil)
   (\"2\" \"Two\"   entry (file \"\") nil)
   (\"3\" \"Three\" entry (file \"\") nil))

Normally template \"Four\" would throw an error because its :keys are not a string."

  (let* ((entries (mapcar #'doct--maybe-convert-form declarations)))
    (unwind-protect
        (progn
          (run-hook-with-args 'doct-after-conversion-hook entries)
          ;;hook functions may set doct-templates to return manipulated list
          (or doct-templates (doct-flatten-lists-in entries)))
      (setq doct-templates nil))))

;;;###autoload
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

For Example:

  (doct-remove-hooks \"^t\" \\='mode t)

Removes and uninterns:

  doct--hook/mode/t
  doct--hook/mode/tt

But not:

  doct--hook/mode/p

From the `org-capture-mode-hook'."
  (interactive)
  (let* ((keys (or keys (read-string "Remove hooks with keys matching: ")))
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
                               (guard (seq-every-p #'symbolp hooks)))
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
        (when (string-match
               keys (car (last (split-string (symbol-name hook-fn) "/"))))
          (remove-hook hook hook-fn)
          (when unintern-functions (unintern hook-fn nil)))))))

(provide 'doct)

;;; doct.el ends here
