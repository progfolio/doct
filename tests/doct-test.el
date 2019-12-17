(require 'ert)

(ert-deftest doct-expands-non-list-entry-to-single-entry ()
  "doct should expand into a single org capture template entry when argument isn't lists"
  (should (equal (doct "test" :keys "t")
                 '("t" "test"))))

(ert-deftest :id-is-exclusive ()
  ":id keyword should override other target keywords if declared first"
  (should (equal (doct "id-test"
                       :keys "t"
                       :id "1"
                       :clock t
                       :function #'identity
                       :file "")
                 '("t" "id-test" (id "1")))))

(ert-deftest :target-is-exclusive ()
  ":target keyword should override no matter where it is declared"
  (should (equal (doct "target-test"
                       :keys "t"
                       :id "1"
                       :clock t
                       :function #'identity
                       :file ""
                       :target t)
                 '("t" "target-test" t))))

(ert-deftest :clock-target-should-not-have-cdr ()
  ":clock keyword shouldn't have a cdr when used as a target."
  (should (equal (doct "clock-test"
                       :keys "t"
                       :clock t)
                 '("t" "clock-test" (clock)))))

(ert-deftest :template-function ()
  "Keyword should properly convert to target entry"
  (should (equal (doct "template-function-test"
                       :keys "t"
                       :template-function #'identity)
                 '("t" "template-function-test" #'identity))))

(ert-deftest nil-additional-option-not-expanded ()
  "Additional options with a nil value should not be included in expanded entry."
  (should (equal (doct "test"
                       :keys "t"
                       :immediate-finish)
                 '("t" "test"))))

(ert-deftest file-without-target-expands-properly()
  "doct shouldn't return a dotted list when its target is a string.
It should return a proper list."
  (let ((form (doct "test"
                    :keys "t"
                    :file "test")))
    (and (should (equal form '("t" "test" (file "test"))))
         (should-not (equal form '("t" "test" (file . "test")))))))

(ert-deftest :parent ()
  "The :parent keyword should compute keys for child entries"
  (should (equal (doct '(("parent" :keys "p")
                         ("child" :keys "c" :parent "parent")
                         ("grandchild" :parent "child" :keys "g")))
                 `(("p" "parent") ("pc" "child") ("pcg" "grandchild")))))

(ert-deftest no-cyclical-parent ()
  "The :parent keyword shouldn't allow entry to be parent of itself,
or compute cyclical parent references."
  (and (should (equal (doct "cyclical" :parent "cyclical" :keys "c")
                      `("c" "cyclical")))
       (should (equal (doct '(("parent" :parent "child" :keys "p")
                              ("child" :parent "parent" :keys "c")))
                      `(("p" "parent") ("pc" "child"))))))
