;; -*- lexical-binding: t -*-
(require 'ert)
(require 'doct)
(require 'org-capture)

(ert-deftest first-file-target-wins ()
  "first file target keyword should override others"
  (should (equal (doct '(("fft-test" :keys "f"
                          :type entry
                          :id "1"
                          :clock t
                          :function identity
                          :target (function identity)
                          :file "")))
                 '(("f" "fft-test" entry (id "1"))))))

(ert-deftest first-file-target-extension-wins ()
  "first file target extension should override others"
  (should (equal (doct `(("ffte-test" :keys "f"
                          :type entry
                          :file ""
                          ;;@HACK, not sure why this test fails if
                          ;;run more than once. The :olp list is
                          ;;nreversed in doct--convert, but
                          ;;multiple invocations of the test should
                          ;;get a fresh copy of the list...
                          :olp ,(seq-copy '("one" "two" "three"))
                          :regexp "one"
                          :headline "one"
                          :function identity)))
                 '(("f" "ffte-test" entry (file+olp "" "one" "two" "three"))))))

(ert-deftest first-template-target-wins ()
  "first template target keyword should override other template target keywords"
  (should (equal (doct '(("ftt-test" :keys "tt"
                          :type entry
                          :id "1"
                          :clock t
                          :function identity
                          :target (function identity)
                          :file "")))
                 '(("tt" "ftt-test" entry (id "1"))))))


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
  ":template-function should properly convert to target entry"
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
