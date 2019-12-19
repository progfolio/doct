;; -*- lexical-binding: t -*-
(require 'ert)
(require 'doct)
(require 'org-capture)
(require 'cl-macs)

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

(ert-deftest :template-is-joined ()
  ":template should join multiple values with a newline"
  (should (equal (doct "template join test"
                       :keys "t"
                       :template "one" "two" "three")
                 '("t" "template join test" "one
two
three"))))

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

;;@FIX
;;State is being held between these tests...
;;We're creating functions with the same symbol name, too
;;which isn't a problem in practice, but doesn't fly with (equal symbol symbol)

;;(ert-deftest doct--add-hook-creates-function-adds-hook ()
;;"doct--add-hook should add a function with the entry's computed keys
;;to the proper hook."
;;(let ((old-org-capture-mode-hook org-capture-mode-hook))
;;(setq org-capture-mode-hook '())
;;(doct `(("parent" :keys "p")
;;("My CHILD"
;;:parent "parent"
;;:keys "c"
;;:hook #'ignore)))
;;(unwind-protect
;;(and (should (functionp #'doct--hook/pc))
;;(should (equal (car org-capture-mode-hook) #'doct--hook/pc)))
;;(setq org-capture-mode-hook old-org-capture-mode-hook)
;;(doct-remove-hooks "pc" 'mode t))))

(ert-deftest doct-remove-hooks-test ()
  "doct-remove-hook should remove doct--hook/ matching regexp for keys
and unintern function."
  (let ((old-org-capture-mode-hook org-capture-mode-hook))
    (setq org-capture-mode-hook '())
    (unintern "doct--hook/pc")

    (doct `(("parent" :keys "p")
            ("child"
             :parent "parent"
             :keys "c"
             :hook #'ignore)))
    (unwind-protect
        (progn
          (doct-remove-hooks "pc" 'mode t)
          (and (should-not (functionp #'doct--hook/pc))
               (should (equal org-capture-mode-hook '()))))
      (unintern "doct--hook/pc")
      (setq org-capture-mode-hook old-org-capture-mode-hook))))

(ert-deftest doct-remove-hooks-from-multiple-targets ()
  "doct-remove-hook should remove doct--hook/ matching regexp for keys
and unintern functions."
  (let* ((hook-keywords '("after-finalize"
                          "before-finalize"
                          "prepare-finalize"
                          "mode"))
         (hooks (mapcar (lambda (keyword)
                          (intern (concat "org-capture-" keyword "-hook")))
                        hook-keywords))
         (old-hooks (mapcar (lambda (hook)
                              (intern (concat "old-" (symbol-name hook))))
                              hooks)))
    ;;@FIX test passes, but old state not being restored.
    ;;must be a better way to encapuslate the state of this test.
    ;;Rethink this...
    ;;~ nv 2019-12-19

    ;;save old hook states
    (cl-loop for old-hook in old-hooks
             for hook in hooks
             do
             (set old-hook (symbol-value hook))
             (set hook nil))

    (unwind-protect
        (progn
          (doct `(("parent" :keys "p")
                  ("child"
                   :parent "parent"
                   :keys "c"
                   :hook #'ignore
                   :after-finalize #'ignore
                   :before-finalize #'ignore
                   :prepare-finalize #'ignore)))

          (doct-remove-hooks "pc" t t)

          (and (should-not (seq-filter
                            'identity
                            (mapcar (lambda (keyword) (functionp
                                                       (intern
                                                        (concat "doct--hook/"
                                                                keyword "/pc"))))
                                    hook-keywords)))
               ;;hooks should be empty
               (should-not (seq-filter 'identity
                                       hooks))))
      (dolist (keyword hook-keywords)
        (unintern (concat "doct--hook/" keyword "/pc")))
      ;;reset hook values
      (cl-loop for hook in hooks
               for old-hook in old-hooks
               do
               (set hook (eval (symbol-value old-hook)))
               (unintern old-hook)))))
