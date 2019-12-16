(require 'ert)

(ert-deftest doct-expands-non-list-entry-to-single-entry ()
  "doct should expand into a single org capture template entry when arguments aren't lists"
  (should (equal (doct "test" :keys "t")
                 '("t" "test"))))

(ert-deftest id-is-exclusive ()
  ":id keyword should override other target keywords if declared first"
  (should (equal (doct :id "1"
                       :clock t
                       :function #'identity
                       :file "")
                 '((id "1")))))

(ert-deftest target-is-exclusive ()
  ":target keyword should override no matter where it is declared"
  (should (equal (doct :id "1"
                       :clock t
                       :function #'identity
                       :file ""
                       :target t)
                 '(t))))

(ert-deftest clock-target-should-not-have-cdr ()
  ":clock keyword shouldn't have a cdr when used as a target."
  (should (equal (doct :clock t)
                 '((clock)))))

(ert-deftest unquote-should-work-within-entries ()
  "Returned entry should have implicit backquote, so user can unquote within entries."
  (should (equal (doct :keys ,(buffer-file-name))
                 `(,(buffer-file-name)))))

(ert-deftest template-function-keyword ()
  "Keyword should properly convert to target entry"
  (should (equal (doct :template-function identity)
                 '(#'identity))))

(ert-deftest nil-additional-option-not-expanded ()
  "Additional options with a nil value should not be included in expanded entry."
  (should (equal (doct "test" :immediate-finish)
                 '("test"))))

(ert-deftest file-without-target-expands-properly()
  "doct shouldn't return a dotted list when its target is a string.
It should return a proper list."
  (let ((form (doct :file "test")))
    (and (should (equal form '((file "test"))))
         (should-not (equal form '((file . "test")))))))
