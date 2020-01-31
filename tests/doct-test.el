;;; doct-test.el --- doct test suite ;; -*- lexical-binding: t -*-
;; Package-Requires: ((buttercup))

;;; Commentary:
;; tests for doct.el

;;; Code:
(require 'doct)
(require 'buttercup)

(describe "DOCT")
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
               :doct-options (:foo t)))))
  (it "allows a child to override its inherited properties."
    (expect (doct '(("parent" :keys "p" :foo t :file ""
                     :children ("child" :keys "c" :foo nil))))
            :to-equal
            '(("p" "parent")
              ("pc" "child" entry (file "") nil
               :doct-options (:foo nil)))))
  (it "allows a child to cancel its inherited properties with nil."
    (expect (doct '(("parent" :keys "p" :foo t :regexp "test" :file "" :headline "test"
                     :children ("child" :keys "c" :foo nil :file nil :function (lambda () (ignore))))))
            :to-equal
            '(("p" "parent")
              ("pc" "child" entry (function (lambda () (ignore))) nil
               :doct-options (:foo nil)))))
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
              '(("t" "Test" entry (file "") nil :doct-options (:inner t :outter t)))))
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
                                :doct-options (:inherited t)))))))
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
  (it "should not return a dotted list when its target is a string."
    (expect (doct '(("test" :keys "t" :type entry :file "")))
            :to-equal
            '(("t" "test" entry (file "") nil))))
  (it "should not have a cdr when :clock is the target."
    (should (equal (doct '(("clock-test" :keys "c"
                            :clock t
                            :template "test")))
                   '(("c" "clock-test" entry (clock) "test"))))))
(describe "Template"
  (it "overrides other template target keywords"
    (expect (doct '(("ftt-test" :keys "tt" :type entry :id "1"
                     :template-file "./template.txt" :template "ignored")))
            :to-equal
            '(("tt" "ftt-test" entry (id "1") (file "./template.txt")))))
  (it "should join multiple strings with a newline"
    (expect (doct '(("template join test" :keys "t" :file ""
                     :template ("one" "two" "three"))))
            :to-equal
            '(("t" "template join test" entry (file "") "one
two
three"))))
  (it "should be returned verbatim when it is a string"
    (expect (doct '(("template join test" :keys "t" :file ""
                     :template "test")))
            :to-equal
            '(("t" "template join test" entry (file "") "test")))))
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
               :doct-options (:custom-option t))))))
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
(describe "%doct(KEYWORD) syntax"
  (before-each (setq org-capture-plist '(:doct-options (:test "passed"))))
  (it "should expand metadata at capture time"
    (expect (funcall (doct--maybe-expand-template-string "it %doct(test)"))
            :to-equal "it passed"))
  (it "should be able to be inlined in another string"
    (expect (funcall (doct--maybe-expand-template-string "it-%doct(test)!"))
            :to-equal "it-passed!")))
(describe "Contexts"
  (before-each (setq org-capture-templates-contexts nil))
  (it "should add single context for a template"
    (doct '(("Context test" :keys "c" :file ""
             :contexts ((:in-buffer "test.org")))))
    (expect org-capture-templates-contexts
            :to-equal '(("c" ((in-buffer . "test.org"))))))
  (it "should add a single inherited context for a template"
    (doct '(("Parent" :keys "p" :contexts ((:in-buffer "test.org"))
             :children ("Child" :keys "c" :file ""))))
    (expect org-capture-templates-contexts
            :to-equal '(("pc" ((in-buffer . "test.org"))))))
  (it "should accept a list of modes per context rule"
    (doct '(("Context test" :keys "c" :file "" :contexts ((:in-mode ("org-mode" "elisp-mode"))))))
    (expect org-capture-templates-contexts
            :to-equal '(("c" (#'(lambda nil
                                  (seq-some
                                   (lambda
                                     (val)
                                     (string-match val
                                                   (symbol-name major-mode)))
                                   '("org-mode" "elisp-mode"))))))))
  (it "should not be added to doct-options"
    (expect (doct '(("Context test" :keys "c" :file ""
                     :contexts ((:in-mode ("org-mode" "elisp-mode"))))))
            :to-equal '(("c" "Context test" entry (file "") nil)))))
(provide 'doct-test)

;;; doct-test.el ends here
