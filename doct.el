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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides an alternative syntax for declaring Org capture
;; templates. See the doct docstring for more details.

;;; Code:
(require 'subr-x)
(require 'seq)
(require 'org-capture)
(require 'warnings)

;;; Custom Options
(defgroup doct nil
  "DOCT: Declarative Org Capture Templates"
  :group 'org)

(defcustom doct-default-entry-type 'entry
  "The default template entry type.
It can be overridden by using the :type keyword in a declaration."
  :type '(choice (const :tag "Regular entry" entry)
                 (const :tag "plain list item" item)
                 (const :tag "checklist item" checkitem)
                 (const :tag "plain text" plain))
  :group 'doct)

(defcustom doct-after-conversion-functions nil
  "Abnormal hook run after converting declarations to templates.
Hook functions are run with the list of templates as their only argument.
The templates have not been flattened at this point and are of the form:
\(((parent) (child)...)...)."
  :group 'doct
  :type 'hook)

(defcustom doct-warn-when-unbound t
  "When non-nil, unbound declaration symbols issue warnings.
Can be overridden on a per-declaration basis by setting :warn."
  :group 'doct
  :type 'boolean)

;;; Variables
(defvar doct-templates nil
  "If non-nil, this is used as the return value of doct.
Use this variable to return an altered list from a function run during
`doct-after-conversion-functions'
Its value is not stored between invocations to doct.")

(defvar doct--current nil
  "The current declaration being processed by doct. Used for error processing.")

(defvar doct--current-plist nil
  "The plist of the current declaration being processed by doct.")

(defvar doct--expansion-syntax-regexp "%doct(\\(.*?\\))"
  "The regular expression for matching keyword in %doct(KEYWORD) template strings.")

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
  "Keywords that define a template's contexts.")

(defvar doct-recognized-keywords `(:children
                                   :contexts
                                   :custom
                                   :disabled
                                   :doct
                                   :doct-name
                                   :keys
                                   :type
                                   :warn
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

;;; Errors
;;doct-error is just parent error symbol.
;;Not intended to be directly signaled.
(define-error 'doct-error               "DOCT peculiar error!")
(define-error 'doct-no-keys             "Declaration has no :keys value" 'doct-error)
(define-error 'doct-group-keys          "Group has :keys value"          'doct-error)
(define-error 'doct-no-target           "Declaration has no target"      'doct-error)
(define-error 'doct-no-template         "Declaration has no template"    'doct-error)
(define-error 'doct-wrong-type-argument "Wrong type argument"            'doct-error)

;;; Utility Functions
(defun doct--get (keyword)
  "Return value for KEYWORD in `doct--current-plist'."
  (plist-get doct--current-plist keyword))

(defun doct--first-in (keywords &optional plist)
  "Find first non-nil occurrence of one of KEYWORDS in PLIST.
If PLIST is nil, `doct--current-plist' is used.
Return (KEYWORD VAL)."
  (let ((target (or plist doct--current-plist)))
    (seq-some (lambda (keyword)
                (when-let ((val (plist-get target keyword)))
                  (when (member keyword keywords)
                    `(,keyword ,val))))
              (seq-filter #'keywordp target))))

(defun doct--plist-p (list)
  "Non-null if and only if LIST is a plist."
  (while (consp list)
    (setq list (if (and (keywordp (car list))
                        (consp (cdr list)))
                   (cddr list)
                 'not-plist)))
  (null list))

(defun doct--list-of-strings-p (object)
  "Return t if OBJECT is a list of strings."
  (and (listp object) (seq-every-p #'stringp object)))

(defun doct--variable-p (object)
  "Return t if OBJECT is a variable symbol."
  (and (symbolp object)
       (not (functionp object))
       (not (keywordp object))
       (not (booleanp object))))

(defun doct--warning-enabled-p ()
  "Return t if `doct-warn-when-unbound' or `doct--current-plist''s :warn is non-nil."
  (if-let (member (plist-member doct--current-plist :warn))
      (cadr member)
    doct-warn-when-unbound))

(defun doct--should-warn-p (object)
  "Return t if `doct--current-plist' satisfies `doct--warning-enabled-p' and OBJECT is unbound."
  (and (doct--variable-p object)
       (not (boundp object))
       (doct--warning-enabled-p)))

(defun doct--maybe-warn (keyword value &optional prefix)
  "Warn for KEYWORD VALUE. If non-nil, PREFIX prefixes message."
  (when (doct--should-warn-p value)
    (lwarn 'doct :warning (concat prefix "%s %s unbound during conversion in declaration:\n %s")
           keyword value doct--current)))

(defun doct--type-check (keyword val predicates &optional current)
  "Type check KEYWORD's VAL.
PREDICATES is a list of predicate functions.
If non-nil, CURRENT is the declaration where an error has occurred.
It defaults to `doct--current'."
  (unless (seq-some (lambda (predicate)
                      (funcall predicate val))
                    predicates)
    (signal 'doct-wrong-type-argument `(,predicates (,keyword ,val)
                                                    ,(or current doct--current))))
  (doct--maybe-warn keyword val))

;;;###autoload
(defun doct-get (keyword)
  "Return KEYWORD's value from `org-capture-plist'.
Checks :doct-custom for KEYWORD and then `org-capture-plist'.
Intended to be used at runtime."
  (let* ((declaration (plist-get org-capture-plist :doct))
         (custom (plist-get declaration :doct-custom)))
    (if-let ((member (plist-member custom keyword)))
        (cadr member)
      (plist-get declaration keyword))))

;;;###autoload
(defun doct-flatten-lists-in (list &optional acc)
  "Flatten each list in LIST. Return ACC.
For example: '((1) ((2 3) (4)) (((5)))) returns: '((1) (2) (3) (4) (5))"
  (dolist (element (nreverse list))
    (if (seq-every-p #'listp element)
        (setq acc (doct-flatten-lists-in element acc))
      (push element acc)))
  acc)

;;; Acessors
;;;; Children
(defun doct--child-list-p (object)
  "Return t when OBJECT is a list but not a function."
  (and (listp object) (not (functionp object))))

(defun doct--children ()
  "Type check and return declaration's :children."
  (let ((children (doct--get :children)))
    (doct--type-check :children children '(doct--child-list-p))
    children))

;;;; Keys
(defun doct--keys (&optional group)
  "Type check and return declaration's :keys.
If GROUP is non-nil, make sure there is no :keys value."
  (let ((keys (plist-member doct--current-plist :keys)))
    (when (and group keys)
      (signal 'doct-group-keys `(,doct--current)))
    (unless (or group keys) (signal 'doct-no-keys `(,doct--current)))
    (let ((keys (cadr keys)))
      (unless (or (stringp keys) group)
        (signal 'doct-wrong-type-argument `(stringp (:keys ,keys) ,doct--current)))
      keys)))

;;;; Entry Type
(defun doct--entry-type ()
  "Return declaration's :type or `doct-default-entry-type'."
  (let ((type (or (doct--get :type) doct-default-entry-type)))
    (or (car (member type doct-entry-types))
        (signal 'doct-wrong-type-argument
                `(,doct-entry-types (:type ,type) ,doct--current)))))

;;;; Target
(defun doct--target-file (value)
  "Convert declaration's :file VALUE and extensions to capture template syntax."
  (doct--type-check :file value '(stringp functionp doct--variable-p))
  (let (type target)
    (pcase (doct--first-in doct-file-extension-keywords)
      (`(:olp ,path) (doct--type-check :olp path '(doct--list-of-strings-p))
       (when (doct--get :datetree)
         (push :datetree type))
       (push :olp type)
       (dolist (heading (nreverse (seq-copy path)))
         (push heading target)))
      (`(:datetree ,_)
       (push :datetree type)
       (push :olp type)
       (when-let ((path (doct--get :olp)))
         (doct--type-check :olp path '(doct--list-of-strings-p))
         (dolist (heading (nreverse (seq-copy path)))
           (push heading target))))
      (`(:function ,fn)
       (doct--type-check :function fn '(functionp doct--variable-p null))
       (push fn target)
       (push :function type))
      (`(,(and (or :headline :regexp) keyword) ,extension)
       (doct--type-check keyword extension '(stringp))
       (push extension target)
       (push keyword type)))
    (push :file type)
    (push value target)
    `(,(intern (mapconcat (lambda (keyword)
                            (substring (symbol-name keyword) 1))
                          (delq nil type) "+"))
      ,@(delq nil target))))

(defun doct--target ()
  "Convert declaration's target to template target."
  (pcase (doct--first-in doct-exclusive-location-keywords)
    ('nil
     (signal 'doct-no-target `(,doct-exclusive-location-keywords nil ,doct--current)))
    (`(:clock ,_) '(clock))
    (`(:id ,id) (doct--type-check :id id '(stringp))
     `(id ,id))
    (`(:function ,fn)
     (doct--type-check :function fn '(functionp doct--variable-p null))
     (if-let ((file (doct--get :file)))
         (doct--target-file file)
       `(function ,fn)))
    (`(:file ,file) (doct--target-file file))))

;;;; Template
(defun doct--replace-template-strings (string declaration)
  "Replace STRING's %doct(KEYWORD) occurrences with their :doct-custom values.
If non-nil, DECLARATION is the declaration containing STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward doct--expansion-syntax-regexp nil :no-error)
        (let* ((keyword (intern (concat ":" (match-string 1))))
               (val (doct-get keyword)))
          (unless (or (stringp val) (null val))
            (lwarn 'doct :warning "%%doct(%s) wrong type: stringp %s in declaration:
%s\nSubstituted for empty string."
                   keyword val declaration)
            (setq val ""))
          (replace-match (or val "")))))
    (buffer-string)))

(defun doct--expansion-syntax-p (string)
  "Return t for STRING containing %doct(keyword) syntax, else nil."
  (and (string-match-p "%doct(.*?)" string) t))

(defun doct--fill-template (&optional value)
  "Fill declaration's :template VALUE at capture time."
  (let* ((declaration (plist-get org-capture-plist :doct))
         (value (or value (plist-get declaration :template)))
         (template (pcase value
                     ((pred stringp) (if (doct--expansion-syntax-p value)
                                         (doct--replace-template-strings
                                          value declaration)
                                       value))
                     ((pred functionp) (doct--fill-template (funcall value)))
                     ((pred doct--list-of-strings-p)
                      (mapconcat (lambda (element)
                                   (if (doct--expansion-syntax-p element)
                                       (doct--fill-template element)
                                     element))
                                 value "\n")))))
    (doct--type-check :template template '(stringp) declaration)
    template))

(defun doct--type-check-template-entry-type (string)
  "Check template STRING against entry type."
  ;;if string is empty, default templates are used.
  (unless (string-empty-p string)
    (pcase (doct--entry-type)
      ('entry
       (unless (string-prefix-p "*" (string-trim string))
         (lwarn 'doct :warning ":template %s in declaration:\n%s
is not a valid Org entry
Are you missing the leading '*'?"
                string doct--current)))
      ('table-line
       (unless (string-empty-p (with-temp-buffer
                                 (insert string)
                                 (goto-char (point-min))
                                 (save-match-data
                                   (flush-lines "\\(?:[[:space:]]*|\\)"))
                                 (buffer-string)))
         (lwarn 'doct :warning ":template %s in declaration:\n%s
contains an invalid table-line
Are you missing the leading pipe?"
                string doct--current))))))

(defun doct--maybe-warn-template (strings)
  "Warn if `doct--should-warn-p' and STRINGS's %doct(KEYWORD) keyword is undeclared."
  (when (doct--warning-enabled-p)
    (let (undeclared
          not-string
          entry-type-mismatches
          template)
      ;;@ENHANCEMENT: ensure keyword's value is a string?
      (dolist (string strings)
        (when (doct--expansion-syntax-p string)
          (with-temp-buffer
            (insert string)
            (goto-char (point-min))
            (save-match-data
              (while (re-search-forward doct--expansion-syntax-regexp nil :no-error)
                (let* ((match   (match-string 1))
                       (keyword (intern (concat ":" match)))
                       (custom  (plist-get doct--current-plist :custom))
                       (member  (or (plist-member custom keyword)
                                    (plist-member doct--current-plist keyword)))
                       (value   (cadr member)))
                  (unless (or member
                              ;;doct implicitly adds this
                              (eq keyword :doct-name))
                    (push (symbol-name keyword) undeclared))
                  (unless (or (stringp value) (null value))
                    (push (symbol-name keyword) not-string))
                  (replace-match (format "%s" value) nil t)
                  (setq string (buffer-string)))))))
        (push string template))
      (doct--type-check-template-entry-type (string-join template "\n"))
      (when (or undeclared not-string entry-type-mismatches)
        (apply #'lwarn
               (delq nil `(doct
                           :warning
                           ,(concat "%%doct(KEYWORD): "
                                    (when undeclared "%s undeclared during conversion\n")
                                    (when not-string "%s did not evaluate to a string\n")
                                    (when entry-type-mismatches
                                      "%s does not meet entry type requirements\n")
                                    "in declaration:\n%s")
                           ,(when undeclared (string-join (nreverse undeclared) ", "))
                           ,(when not-string (string-join (nreverse not-string) ", "))
                           ,(when entry-type-mismatches
                              (string-join (nreverse entry-type-mismatches) ", "))
                           ,doct--current)))))))

(defun doct--template ()
  "Convert declaration's :template to Org capture template."
  (pcase (doct--first-in doct-template-keywords)
    (`(:template-file ,file)
     (doct--type-check :template-file file '(stringp doct--variable-p))
     `(file ,file))
    (`(:template ,template)
     ;;simple values: string, list of strings with no expansion syntax
     (pcase template
       ((and (pred stringp)
             (guard (not (doct--expansion-syntax-p template))))
        (doct--type-check-template-entry-type template)
        template)
       ((and (pred doct--list-of-strings-p)
             (guard (not (seq-some #'doct--expansion-syntax-p template))))
        (doct--type-check-template-entry-type (string-join template "\n"))
        (string-join template "\n"))
       (deferred
         (doct--type-check :template deferred
                           '(functionp stringp doct--list-of-strings-p doct--variable-p))
         (unless (or (functionp deferred) (doct--variable-p deferred))
           (doct--maybe-warn-template
            (if (doct--list-of-strings-p deferred) deferred `(,deferred))))
         '(function doct--fill-template))))))

;;;; Additional Options
(defun doct--validate-option (pair)
  "Type check :KEY VALUE option PAIR declaration."
  (pcase pair
    ;;nil values allowed for overrides. org-capture will just use defaults.
    (`(,(and (or :empty-lines :empty-lines-after :empty-lines-before) option) ,value)
     (doct--type-check option value '(integerp)))
    (`(:table-line-pos ,value)
     (doct--type-check :table-line-pos value '(stringp)))
    (`(:tree-type ,value)
     ;;only a warning because `org-capture-set-target-location'
     ;;has a default if any symbol other than week or month is set
     (unless (member value '(week month))
       (when (doct--warning-enabled-p)
         (lwarn 'doct :warning ":tree-type %s in declaration:\n
%s\n
should be set to week or month, any other values use default datetree type."
                value doct--current))))))

(defun doct--additional-options ()
  "Convert declaration's additional options to Org capture syntax."
  (let (options)
    (dolist (keyword doct-option-keywords options)
      (when-let ((pair (plist-member doct--current-plist keyword)))
        (doct--validate-option pair)
        (setq options (plist-put options (car pair) (cadr pair)))))))

(defun doct--custom-properties ()
  "Return a copy of declaration's :custom plist with unrecognized keywords added."
  (let ((keywords (delete-dups (seq-filter #'keywordp doct--current-plist)))
        custom)
    (dolist (keyword keywords)
      (unless (member keyword doct-recognized-keywords)
        (setq custom (plist-put custom keyword (doct--get keyword)))))
    (if-let ((explicit (doct--get :custom)))
        (progn (doct--type-check :custom explicit '(doct--plist-p))
               (append explicit custom))
      custom)))

;;; External Variables
;;;;Hooks
(defun doct--run-hook (keyword)
  "Run declaration's KEYWORD function."
  (let ((declaration (plist-get org-capture-plist :doct)))
    (when (string= (plist-get declaration :keys)
                   (plist-get org-capture-plist :key))
      (when-let ((fn (plist-get declaration keyword)))
        (funcall fn)))))

;;install hook functinos
(dolist (keyword doct-hook-keywords)
  (let* ((name (substring (symbol-name keyword) 1))
         (fn (eval `(defun ,(intern (concat "doct-run-" name)) ()
                      ,(concat "Run declaration's" name "function.")
                      (doct--run-hook ,keyword))))
         (hook (intern (concat "org-capture-" (if (string= name "hook")
                                                  "mode"
                                                name) "-hook"))))
    (add-to-list hook fn)))

;;;; Contexts
(defun doct--convert-constraint-keyword (keyword)
  "Convert KEYWORD to `org-capture-templates-contexts' equivalent symbol."
  (let ((name (symbol-name keyword)))
    (intern (if (string-prefix-p ":unless" name)
                (replace-regexp-in-string "^:unless" "not-in" name)
              (replace-regexp-in-string "^:" "" name)))))

(defmacro doct--constraint-function (constraint value)
  "CONSTRAINT is a context keyword. VALUE is its value in the current rule."
  (let* ((name (symbol-name constraint))
         (test `(string-match val
                              ,(cond
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
  (let ((form (if (functionp value)
                  `(,value)
                value)))
    `(lambda () (,condition ,form t))))

(defun doct--constraint-rule-list (constraint value)
  "Create a rule list for declaration's CONSTRAINT with VALUE."
  (doct--maybe-warn constraint value ":contexts ")
  `(,(cond
      ((eq constraint :function)
       (doct--type-check :function value '(functionp doct--variable-p))
       value)
      ((or (eq constraint :when) (eq constraint :unless))
       (eval (macroexpand `(doct--conditional-constraint
                            ,(intern (substring (symbol-name constraint) 1))
                            ,value))))
      ((stringp value)
       `(,(doct--convert-constraint-keyword constraint) . ,value))
      ((doct--list-of-strings-p value)
       (macroexpand `(doct--constraint-function ,constraint ,value)))
      (t (signal 'doct-wrong-type-argument
                 `((stringp listp) (:contexts (,constraint ,value))
                   ,doct--current))))))

(defun doct--add-contexts ()
  "Add `org-capture-template-contexts' for current declaration."
  (when-let ((contexts (doct--get :contexts)))
    (let ((keys (doct--keys))
          definitions)
      ;;allow a single, or list, of context definitions
      (dolist (context (if (seq-every-p #'listp contexts) contexts `(,contexts)))
        (if-let ((first (doct--first-in doct-context-keywords context)))
            (let* ((constraint (car first))
                   (value (cadr first))
                   (substitute (plist-get context :keys))
                   (rules (doct--constraint-rule-list constraint value))
                   (definition (delq nil `(,keys ,substitute ,rules))))
              (push definition definitions))
          (signal 'doct-wrong-type-argument `(,@doct-context-keywords nil ,doct--current))))
      (dolist (definition (nreverse definitions))
        (add-to-list 'org-capture-templates-contexts definition)))))

;;; Conversion
(defun doct--inherit (parent child)
  "Inherit PARENT's plist members unless CHILD has already declared them.
The only exceptions to this are the :keys, :children and :group properties.
CHILD's keys are prefixed with PARENT's.
The :children and :group properties are ignored."
  ;;remove :group description
  (when (stringp (car child))
    (pop child))
  (dolist (keyword (seq-filter (lambda (el)
                                 (and (keywordp el)
                                      (not (member el '(:children :group)))))
                               parent))
    (let ((keysp (eq keyword :keys)))
      (unless (and (not keysp) (plist-member child keyword))
        (if keysp
            (plist-put child :keys (concat (plist-get parent :keys)
                                           (plist-get child :keys)))
          (plist-put child keyword (plist-get parent keyword))))))
  child)

(defun doct--compose-entry (keys name parent)
  "Return a template suitable for `org-capture-templates'.
The list is of the form: (KEYS NAME type target template additional-options...).
`doct--current-plist' provides the type, target template and additional options.
If PARENT is non-nil, list is of the form (KEYS NAME)."
  `(,keys ,name
          ,@(unless parent
              `(,(doct--entry-type)
                ,(doct--target)
                ,(doct--template)
                ,@(doct--additional-options)
                :doct (:doct-name ,name
                                  ,@(cdr doct--current)
                                  ,@(when-let ((custom (doct--custom-properties)))
                                      `(:doct-custom ,(doct--custom-properties))))))))

(defun doct--convert (name &rest properties)
  "Convert declaration to a template named NAME with PROPERTIES.
For a full description of the PROPERTIES plist see `doct'."
  (unless (eq (plist-get properties :disabled) t)
    (setq doct--current `(,name ,@properties))
    (setq doct--current-plist properties)
    (let ((group (eq name :group)))
      (unless (or (stringp name) group)
        (signal 'doct-wrong-type-argument
                `(stringp :group ,name ,doct--current)))
      ;;remove :group description
      (when (and group (stringp (car properties)))
        (let ((props (cdr properties)))
          (setq properties props
                doct--current-plist props)))
      (let ((children (doct--children))
            (keys (doct--keys group))
            entry)
        (if children
            (setq children (mapcar (lambda (child)
                                     (apply #'doct--convert
                                            `(,(car child)
                                              ,@(doct--inherit properties
                                                               (cdr child)))))
                                   (if (seq-every-p #'listp children)
                                       children
                                     `(,children))))
          (doct--add-contexts)
          (dolist (keyword doct-hook-keywords)
            (when-let (member (plist-member doct--current-plist keyword))
              (doct--type-check keyword (cadr member)
                                '(functionp doct--variable-p null)))))
        (unless group
          (setq entry (doct--compose-entry keys name children)))
        (if children
            (if group
                `(,@children)
              `(,entry ,@children))
          entry)))))

(defun doct--maybe-convert-declaration (declaration)
  "Attempt to convert DECLARATION to Org capture template syntax."
  (condition-case-unless-debug err
      (apply #'doct--convert declaration)
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

It may be:

  - a string:

  (doct ... :file \"/path/to/target.org\")
  ;;empty string defaults to `org-default-notes-file'
  (doct ... :file \"\")

  - a function:

  ;;lambda
  (doct ... :file (lambda () (concat (read-string \"Org Capture Path: \") \".org\")))
  ;;or a function symbol
  (doct ... :file my/get-file-path)

  - or a variable:

  (doct ... :file my/file-path)

The following keywords refine the target file location:

  - :headline \"node headline\"
    File under unique heading in target file.

  - :olp (\"Level 1 heading\" \"Level 2 heading\"...)
    Define the full outline in the target file.

  - :datetree nil|t
    Requires use of the :file keyword.
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
See \"%doct String Expansion\" below for detail on using that data.

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

A declaration :template may include a keyword's value during capture.
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

Custom keywords take precedence over other declaration keywords.
For example, with:

  (doct \\='((\"Music Gear\" :keys \"m\" :file \"\" :type plain
           :custom (:keys \"Moog\")
           :template \"%doct(keys)\")))

The \"Music Gear\" template expands to \"Moog\" instead of \"m\".
Nil values expand to an empty string.

Hooks
=====

Adding the following keywords in a declaration adds its value to the appropriate \
`org-capture' hook.
The value may be a function or a variable.

  - :hook
    `org-capture-mode-hook'
  - :prepare-finalize
    `org-capture-prepare-finalize-hook'
  - :before-finalize
    `org-capture-before-finalize-hook'
  - :after-finalize
    `org-capture-after-finalize-hook'

For example:

  (doct \\=`((\"example\"
           :keys \"e\"
           :file \"\"
           :hook ,(defun my/fn ()
                   (ignore)))))

runs my/fn during the `org-capture-mode-hook' when selecting the \"example\" template.

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

  (let* ((entries (mapcar #'doct--maybe-convert-declaration declarations)))
    (unwind-protect
        (progn
          (run-hook-with-args 'doct-after-conversion-functions entries)
          ;;hook functions may set doct-templates to return manipulated list
          (or doct-templates (doct-flatten-lists-in entries)))
      (setq doct-templates nil))))

(provide 'doct)

;;; doct.el ends here
