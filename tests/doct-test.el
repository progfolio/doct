;; doct-test.el --- doct test suite ;; -*- lexical-binding: t -*-
;; Package-Requires: ((buttercup))

;;; Commentary:
;; tests for doct.el

;;; Code:
(require 'buttercup)
(require 'cl-lib)
(require 'doct)

(defvar doct-test-type-data '(
                              :character ?c
                              :child-declaration ("child" :keys "c" :file "")
                              :float 1.0
                              :function ignore
                              :integer 1
                              :keyword :keyword
                              :lambda (lambda () (ignore))
                              :list-of-strings ("list" "of" "strings")
                              :mixed-list (mixed list 1)
                              :nil nil
                              :plist (:plist t)
                              :string "string"
                              :t t
                              :unbound-symbol unbound-symbol
                              )
  "List of typed data to for `doct-test-types'.")

(defmacro doct-test-with-templates (templates &rest body)
  "Execute BODY with `org-capture-templates' lexically bound to (doct TEMPLATES)."
  (declare (indent defun))
  `(let ((org-capture-templates (doct ,templates)))
     ,@body))

;;@TODO: implement doct-test-:doct, just returns :doct portion of templates?
;;or doct-test-custom-data just returns :doct-custom for each template

;;@TODO: figure out better way to do error checks
;; we want to check for specific errors...

(defun doct-test-remove-declarations (templates)
  "Remove :doct value from TEMPLATES."
  (let (removed)
    (dolist (template (doct-flatten-lists-in templates))
      (let (copy previous)
        (dolist (element template)
          (if (eq element :doct)
              (setq previous t)
            (unless previous
              (push element copy))))
        (push (nreverse copy) removed)))
    (setq doct-templates (nreverse removed))))

(defun doct-test-without-declarations (templates)
  "Exclude :doct proprety from converted TEMPLATES."
  (let ((doct-after-conversion-functions #'doct-test-remove-declarations))
    (doct templates)))

(defun doct-test-signal-to-message (templates)
  "Work around `debug-on-error' limitation of buttercup.
Buttercup internally sets `debug-on-error' to t.
This conflicts with doct's use of `condition-case-unless-debug'.
Without this, tests that throw will hit the debugger.

Instead of just checking to see if a `user-error' has been signaled,
Convert TEMPLATES and return error message head."
  (let ((debug-on-error nil)
        (doct-error-regexp "\\(?:DOCT \\([^z-a]*?\\):\\)"))
    (condition-case err
        (doct templates)
      (error (when-let ((message (cadr err)))
               (save-match-data
                 (when (string-match doct-error-regexp
                                     (substring-no-properties message))
                   (match-string 1 message))))))))

(defmacro doct-test-warning-message (&rest body)
  "Execute BODY in context of fresh *Warnings* buffer.
Returns *Warnings* `buffer-string'."
  (declare (indent defun))
  ;;Buffer could exist if we've run these test interactively in Emacs.
  (get-buffer-create "*Warnings*")
  (kill-buffer "*Warnings*")
  `(prog1
       (with-current-buffer (get-buffer-create "*Warnings*")
         (buttercup-suppress-warning-capture
           ,@body
           (buffer-string)))
     (kill-buffer "*Warnings*")))

(defun doct-test-always-p (&optional _)
  "Return t."
  t)

(defun doct-test-filled-template (keys)
  "Return filled template string for template starting with KEYS."
  ;;otherwise we get an error because we can't bookmark temp buffer
  (let (org-capture-bookmark)
    (with-temp-buffer
      (org-mode)
      (let ((inhibit-message t))
        (org-capture 0 keys))
      (substring-no-properties (buffer-string)))))

(defun doct-test-select-menu ()
  "Get a list of pairs from the *Org Select* menu.
Each pair is of the form: (KEY TEMPLATE-DESCRIPTION)."
  ;;let* because we want messages inhibited in org-mks call
  (let* ((inhibit-message t)
         (menu-item-regexp "\\[\\(.\\)\\]\\(\\.\\{,3\\}\\)[[:space:]]*\\(.*\\)\\1?")
         (selection-menu
          (catch 'menu
            (cl-letf (((symbol-function 'org-switch-to-buffer-other-window)
                       (lambda (buffer)
                         (set-buffer (get-buffer-create buffer))))
                      ;;Pre 9.2 versions of Org do not have `org--mks-read-key'.
                      ((symbol-function (if (fboundp 'org--mks-read-key)
                                            'org--mks-read-key
                                          'read-char-exclusive))
                       (lambda (&rest _)
                         (throw 'menu (with-current-buffer "*Org Select*"
                                        (buffer-string))))))
              (org-mks (org-contextualize-keys
                        org-capture-templates
                        org-capture-templates-contexts) "Selection Test"))))
         menu-items)
    (with-temp-buffer
      (insert selection-menu)
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward menu-item-regexp nil t)
          (push `(,(match-string 1) ,(match-string 3)) menu-items))))
    (nreverse menu-items)))

(defun doct-replace-type-placeholder (declaration value)
  "Return copy of DECLARATION with TYPE replaced with VALUE."
  (let (copy)
    (dolist (element declaration (nreverse copy))
      (if (eq element 'type)
          (push value copy)
        (when (listp element)
          (setq element (doct-replace-type-placeholder element value)))
        (push element copy)))))

(defun doct-test-types (declaration)
  "Test DECLARATION against values of `doct-test-type-data'."
  ;;suppressing here. Test for warnings separately.
  (buttercup-suppress-warning-capture
    (let  (results)
      (dolist (keyword (seq-filter #'keywordp doct-test-type-data)
                       (sort results (lambda (a b)
                                       (string< (symbol-name a) (symbol-name b)))))
        (let* ((val (plist-get doct-test-type-data keyword))
               (typed (doct-replace-type-placeholder declaration val))
               result
               ;;override buttercup's debug-on-error binding so we don't hit debugger.
               debug-on-error)
          (setq result (condition-case nil
                           (doct `(,typed))
                         (error 'error)))
          (unless (eq result 'error)
            (push keyword results)))))))

(describe "DOCT"
  (before-each
    (setq doct-default-entry-type           'entry
          doct-warn-when-unbound            t
          org-capture-templates-contexts    nil))
  (it "does not mutate the declaration list"
    (expect (let ((declarations '(("Todo" :keys "t"
                                   :template "* TODO %?")
                                  ("Entry" :keys "e"
                                   :template "* TODO %?\n%a"))))
              (doct `(("Clock" :keys "c" :clock t
                       :children ,declarations)
                      ("Inbox" :keys "i" :file "/tmp/inbox.org"
                       :children ,declarations)))
              declarations)
            :to-equal
            '(("Todo" :keys "t"
               :template "* TODO %?")
              ("Entry" :keys "e"
               :template "* TODO %?\n%a"))))
  (describe "name"
    (it "errors if name is not a string or the keyword :group"
      (expect (doct-test-types '(type :keys "t" :file ""))
              :to-equal '(:string)))
    (describe ":group"
      (it "errors if group has a :keys property"
        (expect (let (debug-on-error)
                  (doct '((:group "Test Group" :keys "a"))))
                :to-throw 'user-error))
      (it "allows nested groups"
        (expect (doct-test-without-declarations
                 '((:group "Outter" :outter t
                           :children
                           ((:group "Inner" :inner t :children
                                    ("nested group" :keys "t" :file ""))))))
                :to-equal '(("t" "nested group" entry (file "") nil))))
      (it "allows a group to optionally exclude its description string"
        (expect (doct-test-without-declarations
                 '((:group :children
                           ((:group :children
                                    (":group no description" :keys "t" :file ""))))))
                :to-equal '(("t" ":group no description" entry (file "") nil))))
      (it "does not include group in template list"
        (expect (doct-test-without-declarations
                 '((:group "Test Group" :inherited t :children
                           ("Parent" :keys "p" :children
                            ("Child" :keys "c"
                             :file "")))))
                :to-equal '(("p" "Parent") ("pc" "Child" entry (file "") nil))))))
  (describe ":children"
    (it "errors if is not a list or nil"
      (expect (doct-test-types '(":children type" :keys "t" :file "" :children type))
              :to-equal '(:child-declaration :nil)))
    (it "puts ancestors properties on descendants."
      (expect (doct '(("parent" :keys "p" :foo t :file ""
                       :children ("child" :keys "c"))))
              :to-equal
              '(("p" "parent")
                (#1="pc" #2="child" entry (file #3="") nil
                    :doct (:doct-name #2# :keys "c" :inherited-keys #1# #4=:foo #5=t :file #3#
                                      :doct-custom (#4# #5#))))))
    (it "allows a child to override its inherited properties."
      (expect (doct-test-without-declarations
               '(("parent" :keys "p" :file "test.org"
                  :children ("child" :keys "c" :file ""))))
              :to-equal
              '(("p" "parent")
                ("pc" "child" entry (file "") nil))))
    (it "allows a child to cancel its inherited properties with nil."
      (expect (let* ((templates (doct '(("parent" :keys "p"
                                         :foo t
                                         :regexp "test"
                                         :file ""
                                         :headline "test"
                                         :children ("child" :keys "c"
                                                    :foo nil
                                                    :file nil
                                                    :function (lambda () "pass"))))))
                     (child (nth 1 templates))
                     (target (nth 3 child))
                     (declaration (nth 6 child))
                     (custom (plist-get declaration :doct-custom)))
                `(,(funcall (eval target)) ,custom))
              :to-equal '("pass" (:foo nil))))
    (it "allows a child to cancel exclusive inherited properties"
      (expect (doct-test-without-declarations
               '(("parent" :keys "p" :function (lambda () (ignore))
                  :children ("child" :keys "c" :file "" :function nil))))
              :to-equal
              '(("p" "parent") ("pc" "child" entry (file "") nil)))))

  (describe ":clock"
    (it "exclusively sets target location"
      (expect (doct-test-without-declarations
               '((":clock exclusivity" :keys "c" :clock t
                  ;;remaining keywords ignored
                  :file ""
                  :function (lambda () (ignore))
                  :id "1")))
              :to-equal '(("c" ":clock exclusivity" entry (clock) nil)))))
  (describe ":contexts"
    (before-each (setq org-capture-templates-contexts nil))
    (it "errors if no context rule keyword is found"
      (expect (doct-test-signal-to-message
               '(("Context rule keyword nil" :keys "c" :file ""
                  :contexts (:foo t :keys "oops"))))
              :to-equal "Wrong type argument"))
    ;;@TODO: flesh this out for all the rule keywords. Some accept variables/functions/forms...
    ;;main divide is between general conditional keywords (:when, :unless, :function)
    ;;and :in/unless-:buffer/:mode/:file keywords
    (it "errors if context rule's value is not a string or list of strings"
      (expect (doct-test-types '("Context value test" :keys "cf" :file ""
                                 :contexts (:in-buffer type)))
              :to-equal '(:list-of-strings :string)))
    (it "warns if context rule's value is unbound"
      (expect (doct-test-warning-message
                (doct '((":context rule unbound warning" :keys "c" :file ""
                         :contexts (:when unbound-symbol)))))
              :to-match
              "Warning (doct): :contexts :when unbound-symbol unbound during conversion in declaration:.*"))
    (it "allows a single context rule"
      (doct '(("single context rule" :keys "c" :file ""
               :contexts (:in-buffer "test.org"))))
      (expect org-capture-templates-contexts
              :to-equal '(("c" ((in-buffer . "test.org"))))))
    (it "adds single context for a template"
      (doct '(("add single context" :keys "c" :file ""
               :contexts ((:in-buffer "test.org")))))
      (expect org-capture-templates-contexts
              :to-equal '(("c" ((in-buffer . "test.org"))))))
    (it "adds a single inherited context for a template"
      (doct '(("Parent" :keys "p" :contexts ((:in-buffer "test.org"))
               :children ("Child" :keys "c" :file ""))))
      (expect org-capture-templates-contexts
              :to-equal '(("pc" ((in-buffer . "test.org"))))))
    (it "accepts a list of values per context rule"
      (doct '(("list per context rule" :keys "c" :file ""
               :contexts ((:in-mode ("org-mode" "elisp-mode"))))))
      (expect org-capture-templates-contexts
              :to-equal '(("c" (#'(lambda nil
                                    (seq-some
                                     (lambda
                                       (val)
                                       (string-match val
                                                     (symbol-name major-mode)))
                                     '("org-mode" "elisp-mode"))))))))
    (it "is not added to :doct-custom"
      (expect (doct '((":context not custom" :keys "c" :file ""
                       :custom (:alone t)
                       :contexts ((:in-mode ("org-mode" "elisp-mode"))))))
              :to-equal
              '((#1="c" #2=":context not custom" entry (file #3="") nil
                    :doct (:doct-name #2# :keys #1# :file #3# :custom #4=(:alone t)
                                      :contexts ((:in-mode ("org-mode" "elisp-mode")))
                                      :doct-custom #4#)))))
    (describe ":function"
      (it "errors if value is not a function"
        (expect (doct-test-signal-to-message
                 '(("Context :function test" :keys "cf" :file ""
                    :contexts (:function t))))
                :to-equal "Wrong type argument"))
      (it "only includes templates which pass predicate"
        (setq org-capture-templates-contexts nil)
        (expect (let ((org-capture-templates
                       (doct '((:group
                                :file ""
                                :children
                                (("One"   :keys "1" :contexts (:function (lambda () t)))
                                 ("Two"   :keys "2" :contexts (:function (lambda () nil)))
                                 ("Three" :keys "3")))))))
                  (doct-test-select-menu))
                :to-equal '(("1" "One") ("3" "Three")))))
    (describe ":when"
      (it "accepts a function as its value"
        (expect (let ((org-capture-templates
                       (doct '((":when test" :keys "w" :file "" :contexts (:when doct-test-always-p))))))
                  (doct-test-select-menu))
                :to-equal '(("w" ":when test"))))
      (it "accepts a single form as its value"
        (expect (let ((org-capture-templates
                       (doct '((":when test" :keys "w" :file "" :contexts (:when (or nil t)))))))
                  (doct-test-select-menu))
                :to-equal '(("w" ":when test")))))
    (describe ":unless"
      (it "accepts a function as its value"
        (expect (let ((org-capture-templates
                       (doct '((":unless test" :keys "u" :file "" :contexts (:unless doct-test-always-p))))))
                  (doct-test-select-menu))
                :to-equal nil))
      (it "accepts a single form as its value"
        (expect (let ((org-capture-templates
                       (doct '((":unless test" :keys "u" :file "" :contexts (:unless (or nil t)))))))
                  (doct-test-select-menu))
                :to-equal nil))))
  (describe ":custom"
    (it "adds custom data to :doct-custom"
      (expect (doct '((":custom data" :keys "c" :file "" :implicit t
                       :custom (:keys "Moog"))))
              :to-equal
              '((#1="c" #2=":custom data" entry (file #3="") nil
                    :doct (:doct-name #2# :keys #1# :file #3# #4=:implicit #5=t
                                      :custom (#6=:keys #7="Moog")
                                      :doct-custom (#6# #7# #4# #5#))))))
    (it "errors if value is not a plist or nil"
      (expect (doct-test-types '(":custom type" :keys "t" :file "" :custom type))
              :to-equal '(:nil :plist))))
  (describe ":disabled"
    (it "does not include templates with a :disabled value of t"
      (expect (doct-test-without-declarations
               '(("Enabled"  :keys "e" :file "")
                 ("Disabled" :keys "d" :file "" :disabled t)))
              :to-equal '(("e" "Enabled" entry (file "") nil))))
    (it "does not disable template when :disabled value is not t"
      (expect (doct-test-without-declarations
               '(("Enabled"  :keys "e" :file "")
                 ("Disabled" :keys "d" :file "" :disabled nil)))
              :to-equal '(("e" "Enabled"  entry (file "") nil)
                          ("d" "Disabled" entry (file "") nil))))
    (it "does not error check a disabled template"
      (expect (doct-test-signal-to-message
               (doct '(("Enabled"  :keys "e" :file "")
                       ;;has no :keys
                       ("Disabled" :file "" :disabled t))))
              :not :to-equal 'user-error)))

  (describe ":warn"
    (it "suppresses warning for unbound symbols when :warn is nil"
      (expect (let ((doct-warn-when-unbound t))
                (doct-test-warning-message
                  (doct '(("unbound fn warning test" :keys "u" :type entry :file ""
                           :function unbound-function
                           :warn nil)))))
              :not :to-match "Warning (doct): :function .* unbound during conversion .*"))
    (it "overrides doct-warn-when-unbound"
      (expect (let ((doct-warn-when-unbound nil))
                (doct-test-warning-message
                  (doct '(("unbound fn warning test" :keys "u" :type entry :file ""
                           :warn t
                           :function unbound-function)))))
              :to-match "Warning (doct): :function .* unbound during conversion .*")))

  (describe ":file"
    (it "errors if :file is not a function, string, variable or nil"
      (expect (doct-test-types '(":file type" :keys "f" :file type
                                 ;;id here to test against nil :file
                                 ;;without throwing unrelated no-target error
                                 :id "2"))
              :to-equal '(:function :lambda :nil :string :unbound-symbol)))
    (it "warns when value is unbound during conversion"
      (expect (doct-test-warning-message
                (doct '((":file warn" :keys "f" :type entry :file unbound-file))))
              :to-match "Warning (doct): :file unbound-file unbound during conversion .*"))
    (it "exclusively sets target when not used with :function"
      (expect (doct-test-without-declarations
               '((":file exclusivity" :keys "f" :file ""
                  ;;remaining keywords ignored
                  :clock t
                  :id "1")))
              :to-equal '(("f" ":file exclusivity" entry (file "") nil))))

    (describe ":headline"
      (it "errors if used without :file"
        (expect (doct-test-signal-to-message
                 '((":headline no :file error" :keys "h" :headline "test")))
                :to-equal "Declaration has no target"))
      (it "errors if it is not a string or nil"
        (expect (doct-test-types '(":headline type" :keys "h" :file "" :headline type))
                :to-equal '(:nil :string)))
      (it "combines with :file"
        (expect (doct-test-without-declarations
                 '((":headline + :file" :keys "h" :file "" :headline "headline")))
                :to-equal
                '(("h" ":headline + :file" entry (file+headline "" "headline") nil)))))

    (describe ":olp"
      (it "errors if used without :file"
        (expect (doct-test-signal-to-message
                 '((":olp without :file error" :keys "o" :olp ("one"))))
                :to-equal "Declaration has no target"))
      (it "errors if it is not a list of strings or nil"
        (expect (doct-test-types '(":olp type" :keys "o" :file "" :olp type))
                :to-equal '(:list-of-strings :nil)))
      (it "combines with :file"
        (expect (doct-test-without-declarations
                 '((":olp + :file" :keys "o" :file "" :olp ("one" "two" "three"))))
                :to-equal
                '(("o" ":olp + :file" entry (file+olp "" "one" "two" "three") nil))))

      (describe ":datetree"
        (it "errors if not used with :file"
          (expect (doct-test-signal-to-message
                   '((":datetree without :file error" :keys "d" :datetree t)))
                  :to-equal "Declaration has no target"))
        (it "can be used without specifying :olp"
          (expect (doct-test-without-declarations
                   '((":datetree + :file" :keys "d" :file "" :datetree t)))
                  :to-equal
                  '(("d" ":datetree + :file" entry (file+olp+datetree "") nil))))
        (it "combines with :olp"
          (expect (doct-test-without-declarations
                   '((":datetree + :olp" :keys "d" :file "" :olp ("one" "two" "three")
                      :datetree t)))
                  :to-equal
                  '(("d" ":datetree + :olp" entry
                     (file+olp+datetree "" "one" "two" "three") nil))))))
    (describe ":regexp"
      (it "errors if used without :file"
        (expect (doct-test-signal-to-message
                 '((":regexp without :file warning" :keys "r" :regexp "test")))
                :to-match "Declaration has no target"))
      (it "errors if it is not a string or nil"
        (expect (doct-test-types '(":regexp type" :keys "r" :file "" :regexp type))
                :to-equal '(:nil :string)))
      (it "combines with :file"
        (expect (doct-test-without-declarations
                 '((":regexp + :file" :keys "r" :file "" :regexp "regexp")))
                :to-equal
                '(("r" ":regexp + :file" entry (file+regexp "" "regexp") nil))))))

  (describe ":function"
    (it "errors if it is not a function, variable or nil"
      (expect (doct-test-types '(":function type" :keys "f" :file "" :function type)))
      :to-equal '(:function :lambda :nil :unbound-symbol))
    (it "warns when value is unbound during conversion"
      (expect (doct-test-warning-message
                (doct '((":function warn" :keys "f" :function unbound-symbol))))
              :to-match
              "Warning (doct): :function unbound-symbol unbound during conversion in declaration.*"))
    (it "exlucsively sets target when not used with :file"
      (expect (doct-test-without-declarations
               '((":function exclusivity" :keys "f" :function identity
                  ;;remaining keywords ignored
                  :clock t
                  :id "1")))
              :to-equal
              '(("f" ":function exclusivity" entry (function identity) nil))))
    (it "combines with the :file keyword"
      (expect (doct-test-without-declarations
               '((":function + :file combination" :keys "f"
                  :file "" :function identity)))
              :to-equal
              '(("f" ":function + :file combination" entry
                 (file+function "" identity) nil)))))

  (describe ":id"
    (it "errors if it is not a string or nil"
      (expect (doct-test-types '(":id type" :keys "i" :id type
                                 ;;fallback for when id is nil
                                 :file ""))
              :to-equal '(:nil :string)))
    (it "exclusively sets target"
      (expect (doct-test-without-declarations
               '((":id exclusivity" :keys "i" :id "1"
                  ;;remaining keywords ignored
                  :clock t
                  :file ""
                  :function identity)))
              :to-equal
              '(("i" ":id exclusivity" entry (id "1") nil)))))

  (describe ":keys"
    (it "errors if it is not a string"
      (expect (doct-test-types '(":keys type" :keys type :file ""))
              :to-equal '(:string)))
    (it "prefixes child :keys with ancestors' :keys"
      (expect (doct-test-without-declarations
               '(("parent" :keys "p" :children
                  (("one" :keys "o" :file "")
                   ("two" :keys "t" :file "")))))
              :to-equal
              '(("p" "parent") ("po" "one" entry (file "") nil)
                ("pt" "two" entry (file "") nil)))))

  (describe ":template"
    (it "errors unless it is a string, list of strings, function, variable, or nil"
      (expect (doct-test-types '(":template type" :keys "t" :file "" :template type))
              :to-equal
              '(:function :lambda :list-of-strings :nil :string :unbound-symbol)))
    (it "warns when value is unbound during conversion"
      (expect
       (doct-test-warning-message
         (doct '((":template warning" :keys "t" :file "" :template unbound-symbol))))
       :to-match
       "Warning (doct): :template unbound-symbol unbound during conversion in declaration:.*"))
    (it "exclusively sets template target"
      (expect (doct-test-without-declarations
               '((":template exclusivity" :keys "t" :file ""
                  :template "* test" :template-file "./ignored.txt")))
              :to-equal
              '(("t" ":template exclusivity" entry (file "") "* test"))))
    (it "joins multiple strings with a newline"
      (expect (doct-test-without-declarations
               '(("template join test" :keys "t" :file ""
                  :type plain
                  :template ("one" "two" "three"))))
              :to-equal
              '(("t" "template join test" plain (file "") "one\ntwo\nthree"))))
    (it "is returned verbatim when it is a string"
      (expect (doct-test-without-declarations
               '(("template join test" :keys "t" :file "" :template "* test")))
              :to-equal
              '(("t" "template join test" entry (file "") "* test")))))

  (describe ":template-file"
    (it "errors if it is not a string, variable or nil"
      ;;unbound symbol will warn
      (expect (doct-test-types '(":template-file type" :keys "t" :file ""
                                 :template-file type)))
      :to-equal '(:nil :string :unbound-symbol))
    (it "warns when value is unbound during conversion"
      (expect (doct-test-warning-message
                (doct '((":template-file warning" :keys "f" :file ""
                         :template-file unbound-symbol))))
              :to-match "Warning (doct): :template-file unbound-symbol unbound during conversion in declaration.*"))
    (it "exclusively sets template target"
      (expect (doct-test-without-declarations
               '((":template-file exclusivity" :keys "t" :type entry :file ""
                  :template-file "./template.txt" :template "ignored")))
              :to-equal
              '(("t" ":template-file exclusivity" entry (file "")
                 (file "./template.txt"))))))

  (describe ":type"
    (it "errors if entry type is not a member of `doct-entry-types' or nil"
      (expect (doct-test-types '(":type type" :keys "t" :file "" :type type))
              :to-equal '(:nil)))
    (it "defaults to `doct-default-entry-type' if nil"
      (let* ((doct-default-entry-type 'plain)
             (tests '((("doct-default-entry-type" :keys "t" :file ""))
                      (("doct-default-entry-type" :keys "t" :file "" :type nil)))))
        ;;@TODO: is this actually running twice?
        (dolist (test tests)
          (expect (eval (macroexpand `(doct-test-without-declarations ',test)))
                  :to-equal
                  '(("t" "doct-default-entry-type" plain (file "") nil)))))))
  ;; (describe "Options"
  ;;   (it "overrides additional options for the same keyword"
  ;;     (expect (let (doct-after-conversion-functions)
  ;;               (doct '(("test" :keys "t"
  ;;                        :file ""
  ;;                        :immediate-finish t
  ;;                        :custom-option t
  ;;                        :immediate-finish nil
  ;;                        :custom-option nil)))
  ;;               :to-equal
  ;;               '((#1="t" #2="test" entry (file #3="") nil #4=:immediate-finish #5=t
  ;;                     :doct (#2# :keys #1# :file #3#
  ;;                                #4# #5#
  ;;                                #6=:custom-option #5#
  ;;                                #4# nil
  ;;                                #6# nil
  ;;                                :doct-custom (#6# #5#)))))))
  ;;   (it "errors if :empty-lines is not an interger or nil"
  ;;     (expect (doct-test-signal-to-message
  ;;               (doct '((":empty-lines type error" :keys "e" :file "" :empty-lines "one"))))
  ;;             :to-equal 'user-error))
  ;;   (it "errors if :table-line-pos is not a string or nil"
  ;;     (expect (doct-test-signal-to-message
  ;;               (doct '((":table-line-pos type error" :keys "t" :file "" :table-line-pos symbol))))
  ;;             :to-equal 'user-error))
  ;;   (it "warns if :tree-type is not week or month"
  ;;     (expect (doct-test-warning-message
  ;;               (doct '((":tree-type warning" :warn t :keys "t" :file "" :tree-type weak))))
  ;;             :to-match "Warning (doct): :tree-type weak in declaration:.*")))
  (describe "%doct(KEYWORD)"
    (it "warns when keyword is not declared during conversion"
      (expect (doct-test-warning-message
                (doct '(("%doct(KEYWORD) keyword undeclared" :keys "t" :file ""
                         :template "%doct(undeclared)"))))
              :to-match
              "Warning (doct): %doct(KEYWORD): :undeclared undeclared during conversion
in declaration:.*"))
    (it "warns when multiple keywords are not declared during conversion"
      (expect (doct-test-warning-message
                (doct '(("%doct(KEYWORD) multiple undeclared" :keys "t" :file ""
                         :template "%doct(first)%doct(second)"))))
              :to-match
              "Warning (doct): %doct(KEYWORD): :first, :second undeclared during conversion
in declaration:.*"))
    (it "warns for multiple :template strings"
      (expect (doct-test-warning-message
                (doct '(("%doct(KEYWORD) list undeclared" :keys "t" :file ""
                         :template ("%doct(first)" "%doct(second)")))))
              :to-match
              "Warning (doct): %doct(KEYWORD): :first, :second undeclared during conversion
in declaration:.*"))
    (it "warns when expansion is wrong type during conversion"
      (expect (doct-test-warning-message
                (doct '(("wrong expansion type" :keys "w"
                         :file ""
                         :type plain
                         :template "%doct(number)"
                         :number 1)))))
      :to-match  "Warning (doct): %doct(.*) wrong type: stringp.*")
    (it "warns when type is entry and template is not an entry or empty string"
      (expect (doct-test-warning-message
                (doct '(("template expansion entry type" :keys "t"
                         :file ""
                         :type entry
                         :template "no leading star"))))
              :to-match
              "Warning (doct): :template no leading star in declaration:
.*
is not a valid Org entry
Are you missing the leading '*'?"))
    (it "warns when type is table-line and '|' does not prefix template's lines"
      (expect (doct-test-warning-message
                (doct '(("template table-line entry type" :keys "t"
                         :file ""
                         :type table-line
                         :template "| leading pipe\nno leading pipe"))))
              :to-match
              "Warning (doct): :template | leading pipe\nno leading pipe in declaration:.*"))
    (it "warns when expansion is wrong type at runtime"
      (expect (doct-test-warning-message
                (let* ((org-capture-bookmark)
                       (org-capture-templates
                        (doct '(("wrong expansion type" :keys "w"
                                 :no-save t
                                 :immediate-finish t
                                 :file ""
                                 :type plain
                                 :template "%doct(number)"
                                 :number 1)))))
                  (org-capture 0 "w")))
              :to-match  "Warning (doct): %doct(.*) wrong type: stringp.*"))
    (it "expands metadata at run time"
      (expect (let ((org-capture-templates
                     (doct '(("fill test" :keys "f"
                              :template "* %doct(todo-state) %doct(result)"
                              :file ""
                              :no-save t
                              :todo-state "TODO"
                              :result "WORK"
                              :immediate-finish t
                              :empty-lines 0)))))
                (doct-test-filled-template "f"))
              :to-equal "* TODO WORK\n"))
    (it "expands when inlined in another string"
      (expect (let ((org-capture-templates
                     (doct '(("inline fill test" :keys "i"
                              :template "* %doct(todo-state)-still-%doct(result)s"
                              :file ""
                              :no-save t
                              :todo-state "TODO"
                              :result "work"
                              :immediate-finish t
                              :empty-lines 0)))))
                (doct-test-filled-template "i"))
              :to-equal "* TODO-still-works\n"))
    (it "expands members of doct-recognized-keywords"
      (expect (let ((org-capture-templates
                     ;;:file workaround for older Org versions
                     ;;Org complains if target file isn't in Org mode
                     ;;even if we're just inserting into current buffer
                     (doct '(("%doct(headline) test" :keys "h" :file "/tmp/notes.org"
                              :no-save t
                              :immediate-finish t
                              :empty-lines 0
                              :type plain
                              :headline "PASS"
                              :template "%doct(headline)")))))
                (doct-test-filled-template "h"))
              :to-equal "PASS\n"))
    (it "prefers :doct-custom over :doct"
      (expect (let ((org-capture-templates
                     (doct '(("%doct(headline) test" :keys "h" :file "/tmp/notes.org"
                              :no-save t
                              :immediate-finish t
                              :empty-lines 0
                              :type plain
                              :headline "FAIL"
                              :custom (:headline "PASS")
                              :template "%doct(headline)")))))
                (doct-test-filled-template "h"))
              :to-equal "PASS\n"))
    (it "prefers :doct-custom over :doct w explicit nil"
      (expect (let ((org-capture-templates
                     (doct '(("%doct(headline) test" :keys "h" :file "/tmp/notes.org"
                              :no-save t
                              :immediate-finish t
                              :empty-lines 0
                              :type plain
                              :headline "FAIL"
                              :custom (:headline nil)
                              :template "%doct(headline)")))))
                (doct-test-filled-template "h"))
              ;;Another Org mode difference...
              ;;Older versions add the newline, newer do not.
              :to-match "\n?"))
    (it "queries fill function from :doct"
      (expect
       (let ((org-capture-templates (doct '(("fill override test" :keys "f"
                                             :file ""
                                             :type plain
                                             :template "%doct(num) = 1"
                                             :num "1")))))
         (let ((org-capture-templates
                (doct '(("some other f" :keys "f"
                         :file ""
                         :type plain
                         :template "%doct(num) = 2"
                         :num "2"))))))
         (doct-test-filled-template "f"))
       :to-equal "1 = 1")))
  (describe "Utility functions"
    (describe "doct--get"
      (it "gets a value from `doct--current-plist'"
        (expect (let ((doct--current-plist '(:test t)))
                  (doct--get :test))
                :to-equal t))))
  (describe "Hooks"
    (it "errors if value is not a function, variable or nil"
      (expect (doct-test-types '("hook keyword type" :keys "h"
                                 :file ""
                                 :hook type))
              :to-equal '(:function :lambda :nil :unbound-symbol)))
    (it "warns when value is unbound during conversion"
      (expect (doct-test-warning-message
                (doct '(("unbound hook" :keys "h"
                         :file ""
                         :before-finalize unbound-symbol))))
              :to-match "Warning (doct): :before-finalize unbound-symbol unbound during conversion in declaration:.*"))
    (it "runs hook functions"
      (expect (let ((org-capture-templates
                     (doct '(("hooks" :keys "h"
                              :file ""
                              :immediate-finish t
                              :no-save t
                              :type plain
                              :template ""
                              :hook             (lambda () (insert "capture "))
                              :prepare-finalize (lambda () (insert "prepare "))
                              :before-finalize  (lambda () (insert "before "))
                              :after-finalize   (lambda () (end-of-line) (insert "after")))))))
                (doct-test-filled-template "h"))
              :to-equal "capture prepare before after")))
  (describe "Documentation Examples"
    (it "returns documented value for tl;dr"
      (expect (doct-test-without-declarations
               '(("Parent" :keys "p"
                  :file "~/example.org"
                  :prepend t
                  :template ("* %doct(todo-state) %^{Description}"
                             ":PROPERTIES:"
                             ":Created: %U"
                             ":END:"
                             "%?")
                  :children (("First Child"  :keys "1"
                              :headline   "One"
                              :todo-state "TODO"
                              :hook (lambda () (message "\"First Child\" selected.")))
                             ("Second Child" :keys "2"
                              :headline   "Two"
                              :todo-state "NEXT")
                             ("Third Child"  :keys "3"
                              :headline   "Three"
                              :todo-state "MAYBE")))))
              :to-equal
              '(("p" "Parent")
                ("p1" "First Child"  #1=entry (#2=file+headline #3="~/example.org" "One")
                 #4=#'doct--fill-template #5=:prepend #6=t)
                ("p2" "Second Child" #1# (#2# #3# "Two") #4# #5# #6#)
                ("p3" "Third Child"  #1# (#2# #3# "Three") #4# #5# #6#))))
    (it "returns documented value for :group example"
      (expect (doct-test-without-declarations
               '(("Work" :keys "w" :file "~/org/work.org" :children
                  ((:group "Clocked" :clock-in t :children
                           (("Phone Call" :keys "p" :template "* Phone call with %?")
                            ("Meeting"    :keys "m" :template "* Meeting with %?")))
                   ("Browsing" :keys "b" :template "* Browsing %x")))))
              :to-equal
              '(("w" "Work")
                ("wp" "Phone Call" entry (file "~/org/work.org") "* Phone call with %?" :clock-in t)
                ("wm" "Meeting"    entry (file "~/org/work.org") "* Meeting with %?"    :clock-in t)
                ("wb" "Browsing"   entry (file "~/org/work.org") "* Browsing %x"))))
    (it "returns documented value for :children example"
      (expect (doct-test-without-declarations '(("parent" :keys "p"
                                                 :children
                                                 (("child" :keys "c"
                                                   :children
                                                   (("grandchild" :keys "g"
                                                     :file ""
                                                     :type plain
                                                     :template "test")))))))
              :to-equal
              '(("p" "parent") ("pc" "child") ("pcg" "grandchild" plain (file "") "test"))))
    (it "returns a proper value for inherited properties example"
      (expect (doct-test-without-declarations
               '(("Grandparent" :keys "g"
                  :file "example.org"
                  :children ("Parent" :keys "p"
                             :children ("Child" :keys "c")))))
              :to-equal '(("g"   "Grandparent")
                          ("gp"  "Parent")
                          ("gpc" "Child" entry (file "example.org") nil))))
    (it "returns a proper value for inheritance override example"
      (expect (doct-test-without-declarations
               '(("Grandparent" :keys "g"
                  :file "example.org"
                  :children ("Parent" :keys "p"
                             :file "overridden.org"
                             :children ("Child" :keys "c")))))
              :to-equal '(("g"   "Grandparent")
                          ("gp"  "Parent")
                          ("gpc" "Child" entry (file "overridden.org") nil))))
    (it "returns documented value for :type entry example"
      (expect (let ((doct-default-entry-type 'entry))
                (doct-test-without-declarations
                 '(("example" :keys "e" :type entry :file ""))))
              :to-equal
              '(("e" "example" entry (file "") nil))))
    (it "returns documented value for undeclared type example"
      (expect (let ((doct-default-entry-type 'entry))
                (doct-test-without-declarations
                 '(("example" :keys "e" :file ""))))
              :to-equal
              '(("e" "example" entry (file "") nil))))
    (it "returns documented value for first target keyword example"
      (expect (doct-test-without-declarations
               '(("example"
                  :keys "e"
                  :type entry
                  :clock t
                  ;;ignored because clock is first
                  :function (lambda () (ignore))
                  ;;also ignored
                  :id "1")))
              :to-equal
              '(("e" "example" entry (clock) nil))))
    (it "returns documented value for :custom exmaple"
      (doct '(("Music Gear" :keys "m" :file ""
               :custom (:keys "Moog"))))
      :to-equal
      '((#1="m" #2="Music Gear" entry (file #3="") nil
            :doct (#2# :keys #1# :file #3# :custom #4=(:keys "Moog") :doct-custom #4#))))

    ))

(provide 'doct-test)

;; Local Variables:
;; flycheck-emacs-lisp-load-path: inherit
;; End:

;;; doct-test.el ends here
