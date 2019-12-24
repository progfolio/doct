;; -*- lexical-binding: t -*-
(require 'ert)
(require 'doct)
(require 'org-capture)

(ert-deftest :id-is-exclusive ()
  ":id keyword should override other target keywords if declared first"
  (should (equal (doct '(("id-test" :keys "i"
                          :type entry
                          :id "1"
                          :clock t
                          :function #'identity
                          :file "")))
                 '(("i" "id-test" entry (id "1"))))))

(ert-deftest :target-is-exclusive ()
  ":target keyword should override no matter where it is declared"
  (should (equal (doct '(("target-test" :keys "t"
                          :type entry
                          :id "1"
                          :clock t
                          :function #'identity
                          :file ""
                          :target t)))
                 '(("t" "target-test" entry t)))))

(ert-deftest :clock-target-should-not-have-cdr ()
  ":clock keyword shouldn't have a cdr when used as a target."
  (should (equal (doct '(("clock-test" :keys "c"
                          :type entry
                          :clock t)))
                 '(("c" "clock-test" entry (clock))))))

(ert-deftest :template-is-joined ()
  ":template should join multiple values with a newline"
  (should (equal (doct '(("template join test" :keys "t"
                          :template ("one" "two" "three"))))
                 '(("t" "template join test" entry "one
two
three")))))

(ert-deftest :template-function ()
  "Keyword should properly convert to target entry"
  (should (equal (doct '(("template-function-test" :keys "t"
                          :type entry
                          :template-function identity)))
                 '(("t" "template-function-test" entry #'identity)))))

(ert-deftest nil-additional-option-not-expanded ()
  "Additional options with a nil value should not be included in expanded entry."
  (should (equal (doct '(("test" :keys "t"
                          :type entry
                       :immediate-finish nil)))
                 '(("t" "test" entry)))))

(ert-deftest file-without-target-expands-properly()
  "doct shouldn't return a dotted list when its target is a string.
It should return a proper list."
  (let ((form (doct '(("test" :keys "t"
                       :type entry
                       :file "test")))))
              (should (equal form '(("t" "test" entry (file "test")))))))
