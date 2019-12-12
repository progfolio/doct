;;; doct.el --- DOCT: Declarative Org Capture Templates -*- lexical-binding: t; -*-
;; Copyright (C) 2019

;; Author:  <progfolio@protonmail.com>
;; Keywords: org, convenience
;; URL: https://github.com/progfolio/doct.el
;; Created: December 10, 2019
;; Keywords: org, convenience, capture
;; Package-Requires: ((emacs "26.1"))
;; Version: 0.1

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


;;; Commentary:
;; "A spoon full of sugar helps the medicine go down."
;; Syntatic sugar for Org capture templates. See the `doct' docstring for details.

;;; Code:

(defmacro doct--process-args (args &optional multiple)
  "Collect each arg in ARGS into a list of the form:
\((VALUE...) (KEY VALUE VALUE...) (KEY′ VALUE VALUE...)...).
The car of the list contains positional arguments (those specified before any keywords).
The cdr of the list contains lists associated with each keyword.
If MULTIPLE is non-nil, multiple declarations of the same keyword arg combined.

The resultant list is easily queried with `assq'."
  (declare (debug t))
  (let (ignored
        processed-args
        positional-args
        keyword-list)

    (dolist (arg args)
      (let* ((is-keyword (keywordp arg))
             (repeated-keyword (and is-keyword (assq arg processed-args))))
        (cond
         (repeated-keyword (setq ignored (unless multiple t)))
         (is-keyword (setq keyword-list arg
                           ignored nil)
                     (unless (assq keyword-list processed-args)
                       (push `(,keyword-list) processed-args)))
         (ignored)
         (t (push arg (if keyword-list
                          (cdr (assq keyword-list processed-args))
                        positional-args))))))

    `(,(nreverse positional-args)
            ,@(nreverse (mapcar (lambda (xs)
                                (cons (car xs) (nreverse (cdr xs))))
                              processed-args)))))

  (defun doct--find-first-in-args (keywords args)
    "Find first occurence of one of KEYWORDS in ARGS."
    (seq-some (lambda (arg-list)
                (seq-some (lambda (keyword)
                            (assq keyword `(,arg-list)))
                          keywords))
              args))

  (defmacro doct--process-form (form)
    "Convert a declarative form to the form that `org-capture-templates' expects.
FORM is an unquoted sexp follwing the pattern: ((positional args...) (KEY VALUE...)...)."
    (declare (indent 1))
    (let* ((pargs (macroexpand `(doct--process-args ,form)))
           (target (assq :target pargs))
           (template (when-let ((template (assq :template pargs)))
                       (if (= 2 (length template))
                           ;;skip concatenation if single string
                           (nth 1 template)
                         (list '\, `(string-join
                                     ,(list '\` `,(cdr template)) "\n")))))
           additional-options)

      ;;Build target element of template
      ;;handle exclusive cases first: id, clock, function without file
      (unless target
        (when-let ((exclusive-targets '(:id :clock :function))
                   (found (doct--find-first-in-args exclusive-targets pargs))
                   (keyword (car found)))
          ;; :function keyword can be used with file or by itself
          (unless (or (and (eq keyword :function) (assq :file pargs))
                      ;;keyword without value
                      (not (nth 1 found)))
            (setq target (delq nil
                               `(,(intern (substring (symbol-name keyword) 1))
                                 ,(unless (eq keyword :clock) (nth 1 found))))))))

      (unless target
        (when-let* ((file (cdr (assq :file pargs)))
                    (target-type "file")
                    (target-args file))
          (when-let* ((extensions '(:headline :olp :regexp :function))
                      (extension (doct--find-first-in-args extensions pargs))
                      (keyword (car extension)))
            (setq target-type (concat target-type "+"
                                      ;;remove colon from keyword
                                      (substring (symbol-name keyword) 1))
                  target-args (append target-args (cdr extension)))
            (when (eq keyword :olp)
              (when (nth 1 (assq :datetree pargs))
                (setq target-type (concat target-type "+datetree")))))
          (setq target `(,(intern target-type) ,@target-args))))

      (let ((options '(:clock-in
                       :clock-keep
                       :clock-resume
                       :empty-lines
                       :empty-lines-after
                       :empty-lines-before
                       :immediate-finish
                       :jump-to-captured
                       :kill-buffer
                       :no-save
                       :prepend
                       :table-line-pos
                       :time-prompt
                       :tree-type
                       :unnarrowed)))
        (dolist (keyword options)
          (when-let ((option (assq keyword pargs)))
            ;;only push non-nil options
            (when (nth 1 option)
              (push option additional-options)))))

      (unless template
        (when-let* ((template-keywords '(:template-function :template-file))
                    (found (doct--find-first-in-args template-keywords pargs)))
          (setq template `(,(intern (substring (symbol-name (car found)) (length ":template-")))
                           ,(nth 1 found)))))

      ;;TEMPLATE FORM:
      ;;(keys description type target template [additional options...])
      (delq nil
            `(,(nth 1 (assq :keys pargs))
              ;;name is first positional arg
              ,(or (nth 1 (assq :name pargs))
                   (car (car pargs)))
              ,(nth 1 (assq :type pargs))
              ,(or (nth 1 (assq :target pargs)) target)
              ,template
              ,@(apply 'append additional-options)))))

  (defmacro doct (&rest forms)
    "Specify Org capture templates declaratively.

This doctumentation overlaps `org-capture-templates'. Please read and understand
that doctumentation first.

The doct macro accepts a series of unquoted FORMS and returns an implicitly
backquoted list of org capture template entries. Each form must specify, at a
minimum, a name. The name can either be the first value in the form, or
specified with the :name keyword. The :name keyword overrides the positional
argument. A value for the :keys keyword is required as well. For example:

With a positional name argument:

  (doct (\"An example\" :keys \"a\"))

Or with a :name keyword:

  (doct (\"I'm ignored and optional in this case\"
          :keys \"a\"
          :name \"An Example\"))

Both expand to:

  \\=`((\"a\" \"An example\"))

Forms like these must precede forms that share a common prefix key. e.g.

  (doct (\"Templates accessed by pressing \\='a\\='\" :keys \"a\")
        (\"An example template\" :keys \"ae\"...)
        (\"And so on...\" :keys \"as\"...))

Forms with a capture template must specify a type, target and the template.

The type is specified with the :type keyword and accepts the following symbols:
   entry       an Org node, with a headline.  Will be filed
               as the child of the target entry or as a
               top level entry.
   item        a plain list item, will be placed in the
               first plain list at the target
               location.
   checkitem   a checkbox item.  This differs from the
               plain list item only in so far as it uses a
               different default template.
   table-line  a new line in the first table at target location.
   plain       text to be inserted as it is.

For example:

  (doct (\"An example\"
         :keys \"a\"
         :type entry
         ...))

The target is specified using one of several exclusive keywords:

  :id \"id of existing Org entry\"
    File as child of this entry, or in the body of the entry
    (see `org-get-create-id' in addition to `org-capture-templates')

  :clock t
    File to the entry that is currently being clocked

  :function (lambda () ;visit file and move point to desired location...)
    This keyword is exclusive when used without the :file keyword. If :file is
    used, it is combined with it and expected to find the desired location
    within the file specified by :file.

Keywords in this group are ignored after the first one is declared. e.g.

  (doct (\"An Example\"
         :keys \"e\"
         :clock t
         :function (lambda () (ignore)) ;ignored
         :id \"1\" ;also ignored
         ...))

Expands to:

  \\=`((\"e\" \"An Example\" (clock)...))

The target may also be specified directly using the :target keyword. This
overrides all other target keywords. e.g.

  (doct (... :target \\='(file \"/path/to/target.org\")))

A target file is specified with the :file keyword. e.g.

  (doct (... :file \"/path/to/target.org\"))

The following keywords may be used in combination with the :file keyword:

  :headline \"node headline\"
    File under unique heading in target file.

  :olp \"Level 1 heading\" \"Level 2 heading\"...
    Specify the full outline in the target file.
    If :+datetree has a non-nil value, create a date tree for today's date.
    Use a non-nil :time-prompt property to prompt for a different date.
    Use a non-nil :tree-type property to create a week-tree.

  :regexp \"regexp describing location\"
    File to the entry matching regexp in target file

  :function function-finding-location
    If used in addition to the :file keyword, the value should be a function that
    finds the desired location in that file. If used as an exclusive keyword (see
    above), the function must locate both the target file and move point to the
    desired location.

The template is specified with the :template keyword. It accepts any number of
strings which are joined by a new line in the expansion. e.g.

  (doct (...:template \"* Test\" \"One\" \"Two\"))

Expands to:

  \\=`((...\"Test\\nOne\\nTwo\"))

The template may also be specified using :template-function, which expects a
function returning the template, or :template-file, which expects a path to the
file containing the template. The first template keyword declared overrides any
that follow it.

Additional options may be specified as key value pairs. Option keywords with a
nil value are ignored in the expansion. e.g.

  (doct (...:immediate-finish nil))

Expands to:

  \\=`((...))

ee `org-capture-templates' for a full list of additional options."

    (declare (indent 0))
    (let (body)
      (dolist (form `,forms)
        (push (macroexpand `(doct--process-form ,form)) body))
      (list '\` (nreverse body))))

  (provide 'doct)

;;; doct.el ends here
