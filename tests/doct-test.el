;;; doct-test.el --- doct test suite ;; -*- lexical-binding: t -*-
;; Package-Requires: ((buttercup))

;;; Commentary:
;; tests for doct.el

;;; Code:
(require 'buttercup)
(require 'cl-lib)
(require 'doct)

(defun doct-test-always-p ()
  "Predicate which always returns t."
  t)

(defun doct-test--template-string (keys)
  "Return filled template string for template starting with KEYS."
  ;;otherwise we get an error because we can't bookmark temp buffer
  (let (org-capture-bookmark)
    (with-temp-buffer
      (org-mode)
      (let ((inhibit-message t))
        (org-capture 0 keys))
      (substring-no-properties (buffer-string)))))

(defun doct-test--template-selections ()
  "Get a list of pairs from the *Org Select* menu.
Each pair is of the form: (KEY TEMPLATE-DESCRIPTION)."
  (let ((inhibit-message t)
        (unread-command-events (listify-key-sequence (kbd ""))) (menu-item-regexp "\\[\\(.\\)\\]\\(\\.\\{,3\\}\\)[[:space:]]*\\(.*\\)\\1?")
        (selection-menu
         (catch 'menu
           (cl-letf (((symbol-function 'org-switch-to-buffer-other-window)
                      (lambda (buffer)
                        (set-buffer (get-buffer-create buffer))))
                     ((symbol-function 'org--mks-read-key)
                      (lambda (&rest _)
                        (throw 'menu (save-excursion
                                       (set-buffer "*Org Select*")
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

(describe "DOCT"
  (before-each
    (setq doct-default-entry-type        'entry
          doct-after-conversion-hook     nil
          org-capture-mode-hook          nil
          org-before-finalize-hook       nil
          org-prepare-finalize-hook      nil
          org-after-finalize-hook        nil
          org-capture-templates-contexts nil))
  (describe "Inheritance"
    (it "prefixes children keys with ancestor keys"
      (expect (doct '(("parent" :keys "p"
                       :children
                       (("one" :keys "o" :file "") ("two" :keys "t" :file "")))))
              :to-equal
              '(("p" "parent") ("po" "one" entry (file "") nil)
                ("pt" "two" entry (file "") nil))))
    (it "puts ancestors properties on descendants."
      (expect (doct '(("parent" :keys "p" :foo t :file ""
                       :children ("child" :keys "c"))))
              :to-equal
              '(("p" "parent")
                ("pc" "child" entry (file "") nil
                 :doct-custom (:foo t)))))
    (it "allows a child to override its inherited properties."
      (expect (doct '(("parent" :keys "p" :foo t :file ""
                       :children ("child" :keys "c" :foo nil))))
              :to-equal
              '(("p" "parent")
                ("pc" "child" entry (file "") nil
                 :doct-custom (:foo nil)))))
    (it "allows a child to cancel its inherited properties with nil."
      (expect (doct '(("parent" :keys "p" :foo t :regexp "test" :file "" :headline "test"
                       :children ("child" :keys "c" :foo nil :file nil :function (lambda () (ignore))))))
              :to-equal
              '(("p" "parent")
                ("pc" "child" entry (function (lambda () (ignore))) nil
                 :doct-custom (:foo nil)))))
    (it "allows a child to cancel exclusive inherited properties"
      (expect (doct '(("parent" :keys "p" :function (lambda () (ignore))
                       :children ("child" :keys "c" :file "" :function nil :template nil))))
              :to-equal
              '(("p" "parent") ("pc" "child" entry (file "") nil))))
    (describe "Group"
      (it "errors if group has a :keys property"
        (expect (doct '((:group "Test Group" :keys "a")))
                :to-throw 'user-error))
      (it "allows nested groups"
        (expect (doct '((:group "Outter" :outter t :children
                                ((:group "Inner" :inner t :children
                                         ("Test" :keys "t" :file ""))))))
                :to-equal
                '(("t" "Test" entry (file "") nil :doct-custom (:inner t :outter t)))))
      (it "allows a group to optionally leave off its description string"
        (expect (doct '((:group :children
                                ((:group :children
                                         ("Test" :keys "t" :file ""))))))
                :to-equal
                '(("t" "Test" entry (file "") nil))))
      (it "does not include group in template list"
        (expect (doct '((:group "Test Group"
                                :inherited t
                                :children ("Parent" :keys "p"
                                           :children ("Child" :keys "c"
                                                      :file ""
                                                      :template nil)))))
                :to-equal
                '(("p" "Parent") ("pc" "Child" entry (file "") nil
                                  :doct-custom (:inherited t)))))))
  (describe "Target"
    (it "overrides other target keywords"
      (expect (doct '(("fft-test" :keys "f" :type entry
                       :id "1" :clock t :function identity :file "")))
              :to-equal
              '(("f" "fft-test" entry (id "1") nil))))
    (it "extension overrides other extension keywords"
      (expect (doct '(("ffte-test" :keys "f"
                       :type entry :file "" :olp ("one" "two" "three")
                       :regexp "one" :headline "one" :function identity)))
              :to-equal
              '(("f" "ffte-test"
                 entry (file+olp "" "one" "two" "three") nil))))
    (it "is not a dotted list when its target is a string."
      (expect (doct '(("test" :keys "t" :type entry :file "")))
              :to-equal
              '(("t" "test" entry (file "") nil))))
    (it "has no cdr when :clock is the target."
      (expect (doct '(("clock-test" :keys "c"
                       :clock t
                       :template "test")))
              :to-equal
              '(("c" "clock-test" entry (clock) "test")))))
  (describe "Template"
    (it "overrides other template target keywords"
      (expect (doct '(("ftt-test" :keys "tt" :type entry :id "1"
                       :template-file "./template.txt" :template "ignored")))
              :to-equal
              '(("tt" "ftt-test" entry (id "1") (file "./template.txt")))))
    (it "joins multiple strings with a newline"
      (expect (doct '(("template join test" :keys "t" :file ""
                       :template ("one" "two" "three"))))
              :to-equal
              '(("t" "template join test" entry (file "") "one\ntwo\nthree"))))

    (it "is returned verbatim when it is a string"
      (expect (doct '(("template join test" :keys "t" :file ""
                       :template "test")))
              :to-equal
              '(("t" "template join test" entry (file "") "test"))))
    (it "allows lambdas"
      (expect (let ((org-capture-templates
                     (doct '(("template lambda test" :keys "t"
                              :type plain
                              :file ""
                              :immediate-finish t
                              :no-save t
                              :test "OK"
                              :empty-lines nil
                              :template (lambda () (doct-get :test)))))))
                (doct-test--template-string "t"))
              :to-equal "OK\n"))
    (it "allows named functions"
      (defun doct-test-template ()
        (doct-get :test))
      (expect (let ((org-capture-templates
                     (doct '(("template function test" :keys "t"
                              :type plain
                              :file ""
                              :immediate-finish t
                              :no-save t
                              :test "OK"
                              :empty-lines nil
                              :template doct-test-template)))))
                (doct-test--template-string "t"))
              :to-equal "OK\n")
      (fmakunbound 'doct-test-template)))
  (describe "Options"
    (it "overrides additional options for the same keyword"
      (expect (doct '(("test" :keys "t"
                       :file ""
                       :immediate-finish t
                       :custom-option t
                       :immediate-finish nil
                       :custom-option nil)))
              :to-equal
              '(("t" "test" entry (file "") nil
                 :immediate-finish t
                 :doct-custom (:custom-option t)))))
    (it "adds :custom data to :doct-custom"
      (expect (doct '((":custom test" :keys "c" :file "" :implicit t
                       :custom (:keys "Moog"))))
              :to-equal
              '(("c" ":custom test" entry (file "") nil
                 :doct-custom (:keys "Moog" :implicit t)))))
    (it "errors if :custom's value is not a plist"
      (expect (doct '((":custom test" :keys "c" :file "" :implicit t
                       :custom ("oops"))))
              :to-throw 'user-error)))
  (describe "Type checking"
    (let ((types '(nil
                   t
                   'doct-unbound-symbol
                   #'function
                   :keyword 1 1.0
                   "string"
                   ?c
                   '("list"))))
      (it "errors if name is not a string."
        ;;Removing symbol because we have :keys.
        ;;That case is covered in group keys test.
        (dolist (garbage (seq-remove 'symbolp
                                     (seq-remove 'stringp types)))
          (expect (doct `((,garbage :keys "t" :children ())))
                  :to-throw 'user-error)))
      (it "errors if :keys is not a string."
        (dolist (garbage (seq-remove 'stringp types))
          (expect (doct `(("test" :keys ,garbage)))
                  :to-throw 'user-error)))
      (it "errors if :type is not a valid type symbol."
        ;;consider nil valid, type will be determined by `doct-default-entry-type'.
        (dolist (garbage (remq nil types))
          (expect (doct `(("test" :keys "t" :type ,garbage :file "")))
                  :to-throw 'user-error)))
      (it "errors if :children is not a list."
        (dolist (garbage (seq-remove 'listp types))
          (expect (doct `(("test" :keys "t" :children ,garbage)))
                  :to-throw 'user-error)))
      (it "errors if :file is not a string, function -> string, variable -> string"
        (dolist (garbage (delq nil (seq-remove 'stringp types)))
          (expect (doct `(("test" :keys "t" :file ,garbage)))
                  :to-throw 'user-error)))))
  (describe "Contexts"
    (before-each (setq org-capture-templates-contexts nil))
    (it "allows a single context rule"
      (doct '(("Context test" :keys "c" :file ""
               :contexts (:in-buffer "test.org"))))
      (expect org-capture-templates-contexts
              :to-equal '(("c" ((in-buffer . "test.org"))))))
    (it "adds single context for a template"
      (doct '(("Context test" :keys "c" :file ""
               :contexts ((:in-buffer "test.org")))))
      (expect org-capture-templates-contexts
              :to-equal '(("c" ((in-buffer . "test.org"))))))
    (it "adds a single inherited context for a template"
      (doct '(("Parent" :keys "p" :contexts ((:in-buffer "test.org"))
               :children ("Child" :keys "c" :file ""))))
      (expect org-capture-templates-contexts
              :to-equal '(("pc" ((in-buffer . "test.org"))))))
    (it "accepts a list of values per context rule"
      (doct '(("Context test" :keys "c" :file ""
               :contexts ((:in-mode ("org-mode" "elisp-mode"))))))
      (expect org-capture-templates-contexts
              :to-equal '(("c" (#'(lambda nil
                                    (seq-some
                                     (lambda
                                       (val)
                                       (string-match val
                                                     (symbol-name major-mode)))
                                     '("org-mode" "elisp-mode"))))))))
    (it "is not added to doct-custom"
      (expect (doct '(("Context test" :keys "c" :file ""
                       :contexts ((:in-mode ("org-mode" "elisp-mode"))))))
              :to-equal '(("c" "Context test" entry (file "") nil))))
    (it "errors if no context rule keyword is found"
      (expect (doct '(("Context test" :keys "c" :file ""
                       :contexts (:foo t :keys "oops"))))
              :to-throw 'user-error))
    (it "errors if context rule's value is not a string or list of strings"
      (expect (doct '(("Context :function test" :keys "cf" :file ""
                       :contexts (:in-buffer (2)))))
              :to-throw 'user-error))
    (describe "Rule Keywords"
      (describe ":function"
        (it "errors if value is not a function"
          (expect (doct '(("Context :function test" :keys "cf" :file ""
                           :contexts (:function t))))
                  :to-throw 'user-error))
        (it "only includes templates which pass predicate"
          (setq org-capture-templates-contexts nil)
          (expect (let ((org-capture-templates
                         (doct '((:group
                                  :file ""
                                  :children (("One"   :keys "1" :contexts (:function (lambda () t)))
                                             ("Two"   :keys "2" :contexts (:function (lambda () nil)))
                                             ("Three" :keys "3")))))))
                    (doct-test--template-selections))
                  :to-equal '(("1" "One") ("3" "Three")))))
      (describe ":when"
        (it "accepts a function as its value"
          (expect (let ((org-capture-templates
                         (doct '((":when test" :keys "w" :file "" :contexts (:when doct-test-always-p))))))
                    (doct-test--template-selections))
                  :to-equal '(("w" ":when test"))))
        (it "accepts a single form as its value"
          (expect (let ((org-capture-templates
                         (doct '((":when test" :keys "w" :file "" :contexts (:when (or nil t)))))))
                    (doct-test--template-selections))
                  :to-equal '(("w" ":when test")))))
      (describe ":unless"
        (it "accepts a function as its value"
          (expect (let ((org-capture-templates
                         (doct '((":unless test" :keys "u" :file "" :contexts (:unless doct-test-always-p))))))
                    (doct-test--template-selections))
                  :to-equal nil))
        (it "accepts a single form as its value"
          (expect (let ((org-capture-templates
                         (doct '((":unless test" :keys "u" :file "" :contexts (:unless (or nil t)))))))
                    (doct-test--template-selections))
                  :to-equal nil)))))
  (describe "%doct(KEYWORD) syntax"
    (it "expands metadata at capture time"
      (expect (let ((org-capture-templates
                     (doct '(("fill test" :keys "f"
                              :template "* %doct(todo-state) %doct(result)"
                              :file ""
                              :no-save t
                              :todo-state "TODO"
                              :result "WORK"
                              :immediate-finish t
                              :empty-lines 0)))))
                (doct-test--template-string "f"))
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
                (doct-test--template-string "i"))
              :to-equal "* TODO-still-works\n")))
  (describe ":disabled"
    (it "does not include templates with a :disabled value of t"
      (expect (doct '(("Enabled"  :keys "e" :file "")
                      ("Disabled" :keys "d" :file "" :disabled t)))
              :to-equal '(("e" "Enabled" entry (file "") nil))))
    (it "does not disable template when :disabled value is not t"
      (expect (doct '(("Enabled"  :keys "e" :file "")
                      ("Disabled" :keys "d" :file "" :disabled nil)))
              :to-equal '(("e" "Enabled"  entry (file "") nil)
                          ("d" "Disabled" entry (file "") nil))))
    (it "does not error check a disabled template"
      (expect (doct '(("Enabled"  :keys "e" :file "")
                      ;;has no :keys
                      ("Disabled" :file "" :disabled t)))
              :not :to-throw 'user-error))))
(provide 'doct-test)

;;; doct-test.el ends here
