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
(eval-when-compile (require 'subr-x))
(require 'seq)
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

(defvar doct-warning-types '(unbound
                             template-keyword
                             template-keyword-type
                             template-entry-type
                             template-file
                             option-type)
  "The allowed warning types.")

(defcustom doct-warnings t
  "When non-nil, doct will issue warnings.
Valid values are:
  - t
    warn in all cases
  - nil
    do not warn
Or a list containing any of the following symbols:
  - unbound
      warn when a symbol is unbound during conversion
  - template-keyword
      warn when %{KEYWORD} is not found on the declaration during conversion
  - template-keyword-type
      warn when %{KEYWORD} expansion does not return a string.
  - template-entry-type
      warn when the expanded template does not match the capture template's type
  - template-file
      warn when the :template-file's file is not found during conversion
  - option-type
      warn when additional options are not the proper type
It can be overridden on a per-declaration basis with the :warn keyword."
  :group 'doct
  :type `(choice (const :tag "Enable all warnings" t)
                 (const :tag "Disable all warnings" nil)
                 (set :menu-tag "Some"
                      ,@(mapcar (lambda (x) `(const ,x))
                                doct-warning-types))))

;;; Variables
;;necessary for byte-compiler warnings/pre runtime
(defvar org-directory)
(defvar org-capture-plist)
(defvar org-capture-templates-contexts)
(defvar org-capture-mode-hook nil)
(defvar org-capture-before-finalize-hook nil)
(defvar org-capture-prepare-finalize-hook nil)
(defvar org-capture-after-finalize-hook nil)

(defvar doct-templates nil
  "If non-nil, this is used as the return value of doct.
Use this variable to return an altered list from a function run during
`doct-after-conversion-functions'
Its value is not stored between invocations to doct.")

(defvar doct--current nil
  "The current declaration being processed by doct. Used for error processing.")

(defvar doct--current-plist nil
  "The plist of the current declaration being processed by doct.")

(defvar doct--expansion-syntax-regexp "%{\\(.*?\\)}"
  "The regular expression for matching keyword in %{KEYWORD} template strings.")

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
                                   :inherited-keys
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

(defun doct--unbound-variable-p (object)
  "Return t if OBJECT is an unbound variable."
  (and (doct--variable-p object)
       (not (boundp object))))

(defun doct--warn-warning-symbol-maybe (symbol &optional local)
  "Warn if SYMBOL is not t, nil, or member of `doct-warning-types'.
If LOCAL is non-nil, it is used in place of `doct-warning-types'."
  (unless (member symbol doct-warning-types)
    (lwarn 'doct :warning (concat
                           "Unrecognized warning symbol: %s in "
                           (if local
                               "the \"%s\" declaration\n"
                             "doct-warnings: %s\n")
                           "  Should be t, nil, or a member of doct-warning-types")
           symbol (if local (car doct--current) doct-warnings))))

(defun doct--warning-enabled-p (warning)
  "Return t if WARNING symbol is enabled.
Checks for `doct--current-plist' local warnings before `doct-warnings'."
  (if (member warning doct-warning-types)
      (let* ((local (plist-member doct--current-plist :warn))
             (warnings (if local
                           (cadr local)
                         doct-warnings)))
        (pcase warnings
          ((pred listp)
           (dolist (symbol warnings)
             (doct--warn-warning-symbol-maybe symbol local))
           (member warning warnings))
          ((or 'nil 't) warnings)
          (_ (doct--warn-warning-symbol-maybe warnings local)
             (lwarn 'doct :warning "Enabling all warnings")
             t)))
    (user-error "Unrecognized warning symbol %s" warning)))

(defun doct--warn-symbol-maybe (keyword value &optional prefix)
  "Warn for unbound KEYWORD VALUE. If non-nil, PREFIX prefixes message."
  (and (doct--warning-enabled-p 'unbound)
       (doct--unbound-variable-p value)
       (lwarn 'doct :warning (concat prefix
                                     "%s %s unbound during conversion "
                                     "in the \"%s\" declaration")
              keyword value (car doct--current))))

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
  (doct--warn-symbol-maybe keyword val))

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
  (let ((keys (plist-member doct--current-plist :keys))
        (inherited (plist-member doct--current-plist :inherited-keys)))
    (when (and group keys)
      (signal 'doct-group-keys `(,doct--current)))
    (unless (or group keys inherited) (signal 'doct-no-keys `(,doct--current)))
    (let ((keys (or (cadr inherited) (cadr keys))))
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
       (dolist (heading (nreverse path))
         (push heading target)))
      (`(:datetree ,_)
       (push :datetree type)
       (push :olp type)
       (when-let ((path (doct--get :olp)))
         (doct--type-check :olp path '(doct--list-of-strings-p))
         (dolist (heading (nreverse path))
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
(defun doct--replace-template-strings (string &optional declaration)
  "Replace STRING's %{KEYWORD} occurrences with their :doct-custom values.
If non-nil, DECLARATION is the declaration containing STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward doct--expansion-syntax-regexp nil :no-error)
        (let* ((keyword (intern (concat ":" (match-string 1))))
               (val (doct-get keyword)))
          (unless (or (functionp val) (stringp val) (null val))
            (lwarn 'doct :warning
                   (concat
                    "%%{%s} wrong type: stringp %s"
                    (when declaration " in the \"%s\" declaration")
                    "\n  Substituted for empty string.")
                   keyword val declaration)
            (setq val ""))
          (replace-match (if (functionp val)
                             (doct--replace-template-strings (funcall val) declaration)
                           (or val "")))))
      (buffer-string))))

(defun doct--upgrade-expansion-syntax-maybe (value)
  "Upgrade declaration's template VALUE to current `doct--expansion-syntax-regexp'."
  (let ((old-regexp "%doct(\\(.*?\\))"))
    (cond
     ((and (stringp value) (string-match-p old-regexp value))
      (let ((upgraded (replace-regexp-in-string old-regexp "%{\\1}" value)))
        (lwarn 'doct :warning "Old template expansion syntax detected. Upgrading :template value.
Substitute \"%s\" for \"%s\" in your configuration to prevent this warning in the future."
               value upgraded)
        upgraded))
     ((doct--list-of-strings-p value)
      (mapcar #'doct--upgrade-expansion-syntax-maybe value))
     (t value))))

(defun doct--expansion-syntax-p (string)
  "Return t for STRING containing %{KEYWORD} syntax, else nil."
  (and (string-match-p doct--expansion-syntax-regexp string) t))

(defun doct--fill-template (&optional value)
  "Fill declaration's :template VALUE at capture time."
  (let* ((declaration (plist-get org-capture-plist :doct))
         (value (doct--upgrade-expansion-syntax-maybe
                 (or value (plist-get declaration :template))))
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
  (when (doct--warning-enabled-p 'template-entry-type)
    (let ((trimmed (string-trim string)))
      ;;default templates are used when STRING is empty
      (unless (or (string-empty-p trimmed)
                  ;;arbitrary text can be inserted with these patterns
                  (string-match-p (concat "^" doct--expansion-syntax-regexp)
                                  trimmed)
                  (string-prefix-p "%(" trimmed)
                  (string-prefix-p "%[" trimmed))
        (pcase (doct--entry-type)
          ('entry
           (unless (string-prefix-p "* " trimmed)
             (lwarn 'doct :warning
                    (concat
                     "expanded :template \"%s\" in the \"%s\" declaration "
                     "is not a valid Org entry.\n"
                     "  Are you missing the leading '*'?")
                    string (car doct--current))))
          ('table-line
           (unless (string-empty-p (with-temp-buffer
                                     (insert string)
                                     (goto-char (point-min))
                                     (save-match-data
                                       (flush-lines "\\(?:[[:space:]]*|\\)"))
                                     (buffer-string)))
             (lwarn 'doct :warning
                    (concat
                     ":template \"%s\" in the \"%s\" declaration "
                     "is not a valid table-line.\n"
                     "  Are you missing the leading pipe?")
                    string (car doct--current)))))))))

(defun doct--warn-template-maybe (&optional undeclared not-string)
  "If UNDECLARED or NOT-STRING are non-nil, issue appropriate warning."
  (when (or undeclared not-string)
    (apply #'lwarn
           (delq nil
                 `(doct
                   :warning
                   ,(concat "%%{KEYWORD} in the \"%s\" declaration:\n"
                            (when undeclared "  %s undeclared during conversion")
                            (when not-string
                              (concat
                               (when undeclared "\n")
                               "  %s did not evaluate to a string")))
                   ,(car doct--current)
                   ,(when undeclared (string-join (nreverse undeclared) ", "))
                   ,(when not-string (string-join (nreverse not-string) ", ")))))))

(defun doct--validate-template-maybe (strings)
  "Check STRINGS to make sure it is a proper template."
  (let (undeclared
        not-string
        template)
    (catch 'deferred
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
                  ;;If the value is a function, we can't reliably validate during
                  ;;conversion. It may rely on runtime context.
                  (when (functionp value) (throw 'deferred nil))
                  (when (doct--warning-enabled-p 'template-keyword)
                    (unless (or member
                                ;;doct implicitly adds these
                                (member keyword '(:inherited-keys :doct-name)))
                      (push (symbol-name keyword) undeclared)))
                  (when (doct--warning-enabled-p 'template-keyword-type)
                    (unless (or (stringp value) (null value))
                      (push (symbol-name keyword) not-string)))
                  (replace-match (format "%s" value) nil t)
                  (setq string (buffer-string)))))))
        (push string template))
      (doct--type-check-template-entry-type (string-join (nreverse template) "\n"))
      (doct--warn-template-maybe undeclared not-string))))

(defun doct--template ()
  "Convert declaration's :template to Org capture template."
  (pcase (doct--first-in doct-template-keywords)
    (`(:template-file ,file)
     (doct--type-check :template-file file '(stringp doct--variable-p))
     (when (and (stringp file) (doct--warning-enabled-p 'template-file))
       (unless (file-exists-p (expand-file-name file org-directory))
         (lwarn 'doct :warning
                ":template-file \"%s\" not found during conversion in the \
\"%s\" declaration"
                file (car doct--current))))
     `(file ,file))
    (`(:template ,template)
     ;;simple values: string, list of strings with no expansion syntax
     (setq template (doct--upgrade-expansion-syntax-maybe template))
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
           (doct--validate-template-maybe
            (if (doct--list-of-strings-p deferred) deferred `(,deferred))))
         '(function doct--fill-template))))))

;;;; Additional Options
(defun doct--validate-option (pair)
  "Type check :KEY VALUE option PAIR declaration."
  (pcase pair
    ;;nil values allowed for overrides. org-capture will just use defaults.
    (`(,(and (or :empty-lines :empty-lines-after :empty-lines-before) option) ,value)
     (doct--type-check option value '(integerp null)))
    (`(:table-line-pos ,value)
     (doct--type-check :table-line-pos value '(stringp null)))
    (`(:tree-type ,value)
     ;;only a warning because `org-capture-set-target-location'
     ;;has a default if any symbol other than week or month is set
     (unless (member value '(week month nil))
       (when (doct--warning-enabled-p 'option-type)
         (lwarn 'doct :warning (concat
                                ":tree-type %s in the \"%s\" declaration "
                                "should be set to week or month.\n"
                                "  Any other values use the default datetree type.")
                value (car doct--current)))))))

(defun doct--additional-options ()
  "Convert declaration's additional options to Org capture syntax."
  (let (options)
    (dolist (keyword doct-option-keywords options)
      (when-let ((pair (plist-member doct--current-plist keyword)))
        (let ((key (car pair))
              (val (cadr pair)))
          (doct--validate-option `(,key ,val))
          (setq options (plist-put options key val)))))))

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
  (when-let ((fn (doct-get keyword)))
    (funcall fn)))

;;install hook functions
(dolist (keyword doct-hook-keywords)
  (let* ((name (substring (symbol-name keyword) 1))
         (fn (fset (intern (concat "doct-run-" name))
                   (apply-partially #'doct--run-hook keyword)))
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
  (doct--warn-symbol-maybe constraint value ":contexts ")
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
    (if (member keyword '(:inherited-keys :keys))
        (plist-put child :inherited-keys (concat
                                          (or (plist-get parent :inherited-keys)
                                              (plist-get parent :keys))
                                          (plist-get child :keys)))
      (unless (plist-member child keyword)
        (plist-put child keyword (plist-get parent keyword)))))
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

(defun doct--convert-declaration-maybe (declaration)
  "Attempt to convert DECLARATION to Org capture template syntax."
  (condition-case-unless-debug err
      (apply #'doct--convert declaration)
    (doct-error (user-error "DOCT %s" (error-message-string err)))))

;;;###autoload
(defun doct (declarations)
  "DECLARATIONS may be a single declaration or a list of declarations.
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

  %{KEYWORD}

will insert the value declared with :KEYWORD in the template.
For example, with:

  (doct \\='((\"Parent\" :keys \"p\"
           :file \"\"
           :template \"* %{todo-state} %?\"
           :children ((\"One\" :keys \"1\" :todo-state \"TODO\")
                      (\"Two\" :keys \"2\" :todo-state \"IDEA\")))))

Each child template has its :todo-state value expanded in the inherited \
:template.

Values should be strings, functions or nil.

  (doct \\='((\"%{string}\" :keys \"s\" :type plain :file \"\"
           :string \"string\"
           :template \"%{string}\")))

Is replaced with \"string\".

  (doct \\='((\"%{fn}\" :keys \"f\" :type plain :file \"\"
           :fn (lambda () \"string returned from function\")
           :template \"%{fn}\")))

Is replaced with \"string returned from function\".

  (doct \\='((\"%{nil}\" :keys \"f\" :type plain :file \"\"
           :nil nil
           :template \"%{nil}\")))

Is replaced with the empty string \"\".

Custom keywords take precedence over other declaration keywords.
For example, with:

  (doct \\='((\"Music Gear\" :keys \"m\" :file \"\" :type plain
           :custom (:keys \"Moog\")
           :template \"%{keys}\")))

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

  (let* ((declarations
          ;;allow a single declaration or a list of declarations
          (if (seq-every-p #'listp declarations) declarations `(,declarations)))
         (entries (mapcar #'doct--convert-declaration-maybe (copy-tree declarations))))
    (unwind-protect
        (progn
          (run-hook-with-args 'doct-after-conversion-functions entries)
          ;;hook functions may set doct-templates to return manipulated list
          (or doct-templates (doct-flatten-lists-in entries)))
      (setq doct-templates nil))))

(provide 'doct)

;;; doct.el ends here
