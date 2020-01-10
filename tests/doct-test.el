;;; doct-test.el --- doct test suite ;; -*- lexical-binding: t -*-

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
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'ert)
(require 'doct)
(require 'org-capture)

(ert-deftest doct--get-no-pair ()
  "Should return first explicitly set keyword value."
  (should (equal (doct--get '(:foo t :doct-parent (:foo nil)) :foo)
                 '(t))))

(ert-deftest doct--get-pair ()
  "Should return first explicitly set keyword and its value."
  (should (equal (doct--get '(:foo t :doct-parent (:foo nil)) :foo t)
                 '(:foo (t)))))

(ert-deftest doct--additive-keyword ()
  "Given a keyword, return an additive keyword."
  (should (equal (doct--additive-keyword :foo) :+foo)))

(ert-deftest doct--normalize-keyword ()
  "Given an additive keyword, return a normalized keyword."
  (should (equal (doct--normalize-keyword :+foo) :foo)))

(ert-deftest doct--get-additive-keyword ()
  "Additive keywords should inherit and extend their value."
  (should (equal (doct '(("Grandparent" :keys "g"
                          :file ""
                          :template "* "
                          :children ("Parent" :keys "p"
                                     :+template "TODO %? "
                                     :children ("Child" :keys "c"
                                                :+template ":child:")))))
                 '(("g" "Grandparent")
                   ("gp" "Parent")
                   ("gpc" "Child" entry (file "") "* TODO %? :child:")))))

(ert-deftest first-file-target-wins ()
  "first file target keyword should override others"
  (should (equal (doct '(("fft-test" :keys "f"
                          :type entry
                          :id "1"
                          :clock t
                          :function identity
                          :file "")))
                 '(("f" "fft-test" entry (id "1") nil)))))

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
                 '(("f" "ffte-test"
                    entry
                    (file+olp "" "one" "two" "three")
                    nil)))))

(ert-deftest first-template-target-wins ()
  "first template target keyword should override other template target keywords"
  (should (equal (doct '(("ftt-test" :keys "tt"
                          :type entry
                          :id "1"
                          :clock t
                          :function identity
                          :template-file "./template.txt"
                          :file "")))
                 '(("tt" "ftt-test" entry (id "1") #'ignore)))))


(ert-deftest :clock-target-should-not-have-cdr ()
  ":clock keyword shouldn't have a cdr when used as a target."
  (should (equal (doct '(("clock-test" :keys "c"
                          :clock t
                          :template "test")))
                 '(("c" "clock-test" entry (clock) "test")))))

(ert-deftest :template-is-joined ()
  ":template should join multiple values with a newline"
  (should (equal (doct '(("template join test" :keys "t"
                          :file ""
                          :template ("one" "two" "three"))))
                 '(("t" "template join test" entry (file "") "one
two
three")))))

(ert-deftest :template-is-string ()
  ":template should be returned verbatim when it is a string"
  (should (equal (doct '(("template join test" :keys "t"
                          :template "test"
                          :file "")))
                 '(("t" "template join test" entry (file "") "test")))))

(ert-deftest additional-option-not-duplicated ()
  "If declared multiple times, first additional option value is returned once."
  (should (equal (doct '(("test" :keys "t"
                          :type entry
                          :file ""
                          :immediate-finish t
                          :custom-option t
                          :immediate-finish nil
                          :custom-option nil)))
                 '(("t" "test"
                    entry
                    (file "")
                    nil
                    :immediate-finish t
                    :doct-options (:custom-option (t)))))))

(ert-deftest file-without-target-is-proper-list ()
  "doct shouldn't return a dotted list when its target is a string.
It should return a proper list."
  (let ((form (doct '(("test" :keys "t"
                       :type entry
                       :file "")))))
    (should (equal form '(("t" "test" entry (file "") nil))))))

(ert-deftest childern-inherit-keys ()
  "Each child should inherit its parent's keys as a prefix to its own keys."
  (should (equal (doct '(("parent" :keys "p"
                          :children
                          (("one" :keys "o" :file "")
                           ("two" :keys "t" :file "")))))
                 '(("p" "parent")
                   ("po" "one" entry (file "") nil)
                   ("pt" "two" entry (file "") nil)))))

(ert-deftest children-inherit-properties ()
  "Each child should inherit its ancestor's properties."
  (should (equal (doct '(("parent" :keys "p"
                          :foo t
                          :file ""
                          :children ("child" :keys "c"))))
                 '(("p" "parent")
                   ("pc" "child"
                    entry
                    (file "")
                    nil
                    :doct-options (:foo (t)))))))

(ert-deftest childs-properties-override-ancestors ()
  "If a child has a property set it should override that inherited property."
  (should (equal (doct '(("parent" :keys "p"
                          :foo t
                          :file ""
                          :children ("child" :keys "c" :foo nil))))
                 '(("p" "parent")
                   ("pc" "child"
                    entry
                    (file "")
                    nil
                    :doct-options (:foo (nil)))))))

;;error handling
(let ((types '(nil t 'doct-unbound-symbol #'function :keyword 1 1.0 "string" ?c '("list"))))
  (ert-deftest name-type-error ()
    "Error if name isn't a string."
    (dolist (garbage (seq-remove 'stringp types))
      (should-error (doct `((,garbage :keys "t" :children ()))) :type 'user-error)))

  (ert-deftest keys-type-error ()
    "Error if :keys isn't a string."
    (dolist (garbage (seq-remove 'stringp types))
      (should-error (doct `(("test" :keys ,garbage))) :type 'user-error)))

  (ert-deftest entry-type-error ()
    "Error if :type isn't a valid type symbol."
    ;;nil is valid, type will be determined by `doct-default-entry-type'.
    (dolist (garbage (remq nil types))
      (should-error (doct `(("test" :keys "t" :type ,garbage :file "")))
                    :type 'user-error)))

  (ert-deftest target-file-type-error ()
    "Error if :file is not:
- a string
- a function returning a file path
- a or variable evaluating to a file path."
    (dolist (garbage (seq-remove 'stringp types))
      (should-error (doct `(("test" :keys "t" :file ,garbage)))
                    :type 'user-error))))
