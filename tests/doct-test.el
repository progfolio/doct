(require 'ert)

(ert-deftest id-is-exclusive ()
  ":id keyword should override other target keywords if declared first"
  (should (equal (macroexpand-1 (doct (
                                      :id "1"
                                      :clock t
                                      :function #'identity
                                      :file "")))
                 '(((id "1"))))))

(ert-deftest target-is-exclusive ()
  ":target keyword should override no matter where it is declared"
  (should (equal (macroexpand-1 (doct (
                                      :id "1"
                                      :clock t
                                      :function #'identity
                                      :file ""
                                      :target t)))
                 '((t)))))

(ert-deftest clock-target-should-not-have-value ()
  ":clock keyword shouldn't have a cdr when used as a target."
  (should (equal (macroexpand-1 (doct (:clock t)))
                 '(((clock))))))

(ert-deftest unquote-should-work-within-forms ()
  "Returned form should have implicit backquote, so user can unquote within forms."
  (should (equal (macroexpand (doct (:keys ,(buffer-file-name))))
                 (list `(,(buffer-file-name))))))

(ert-deftest template-function-keyword ()
  "Keyword should properly convert to target form"
  (should (equal (macroexpand (doct (:template-function identity)))
                 `((#'identity)))))
