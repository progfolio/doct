;;; doct.el --- DOCT: Declarative Org Capture Templates -*- lexical-binding: t -*-
;; Copyright (C) 2019

;; Author: nv <progfolio@protonmail.com>
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
;; Syntatic sugar for Org capture templates. See the `doct' docstring for details.

;;; Code:

(defun doct--process-args (&rest args)
  "Collect each arg in ARGS into a list of the form:
\((VALUE...) (KEY VALUE VALUE...)...).
The car contains positional arguments (those specified before any keywords).
The cdr contains lists associated with each keyword.

The resultant list is easily queried with `assq'."

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
      ,@(nreverse (mapcar (lambda (arg)
                            (cons (car arg) (nreverse (cdr arg))))
                          processed-args)))))

(defun doct--find-first-in-args (keywords args)
  "Find first occurence of one of KEYWORDS in ARGS."
  (seq-some (lambda (arg-list)
              (seq-some (lambda (keyword)
                          (assq keyword `(,arg-list)))
                        keywords))
            args))

(defun doct--get (arglist &optional keyword)
  "If KEYWORD is non-nil assume ARGLIST is returned by `doct--process-args'.
Otherwise, assume ARGLIST is of the form: (KEYWORD VALUE).
Return value associated with KEYWORD in either case."
  (let ((target (if keyword
                    (assq keyword arglist)
                  arglist)))
    (nth 1 target)))

(defun doct--convert-to-template-entry (form)
  "Convert a declarative form to the entry `org-capture-templates' expects.
FORM is an unquoted sexp of the pattern: (positional args... KEY VALUE...)."

  (let* ((pargs (apply #'doct--process-args form))
         (target (assq :target pargs))
         (template (when-let ((template (assq :template pargs)))
                     (if (= 2 (length template))
                         ;;skip concatenation if single string
                         (doct--get template)
                       (string-join (cdr template)"\n"))))
         (options '(:clock-in
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
                    :unnarrowed))
         (exclusive-targets '(:id :clock :function))
         (target-extensions '(:headline
                              :olp
                              :regexp
                              :function))
         (doct-keywords '(:clock
                          :file
                          :keys
                          :name
                          :target
                          :template
                          :template-file
                          :template-function
                          :type))
         additional-options
         unrecognized-options)

    ;;Build target element of template
    ;;handle exclusive cases first: id, clock, function without file
    (unless target
      (when-let ((found (doct--find-first-in-args exclusive-targets pargs))
                 (keyword (car found)))
        ;; :function keyword can be used with file or by itself
        (unless (or (and (eq keyword :function) (assq :file pargs))
                    ;;keyword without value
                    (not (doct--get found)))
          (setq target (delq nil
                             `(,(intern (substring (symbol-name keyword) 1))
                               ,(unless (eq keyword :clock) (doct--get found))))))))

    (unless target
      (when-let* ((file (doct--get pargs :file))
                  (target-type "file")
                  (target-args file))
        (when-let* ((extension (doct--find-first-in-args target-extensions pargs))
                    (keyword (car extension)))
          (setq target-type (concat target-type "+"
                                    ;;remove colon from keyword
                                    (substring (symbol-name keyword) 1))
                target-args `(,target-args ,@(cdr extension)))
          (when (eq keyword :olp)
            (when (doct--get pargs :datetree)
              (setq target-type (concat target-type "+datetree")))))
        (setq target `(,(intern target-type) ,@`(,target-args)))))

    (dolist (keyword options)
      (when-let ((option (assq keyword pargs)))
        ;;only push non-nil options
        (when (doct--get option)
          (push option additional-options))))

    (unless template
      (when-let* ((template-keywords '(:template-function :template-file))
                  (found (doct--find-first-in-args template-keywords pargs)))
        (pcase  (car found)
          (:template-function (setq template `(function ,(doct--get found))))
          (:template-file (setq template `(file ,(doct--get found)))))))

    ;;TEMPLATE FORM:
    ;;(keys description type target template [additional options...])
    (delq nil
          `(,(doct--get pargs :keys)
            ;;description is first positional arg
            ;;:name for a position independent alias
            ,(or (doct--get pargs :name)
                 (caar pargs))
            ,(doct--get pargs :type)
            ,(or (doct--get pargs :target) target)
            ,template
            ,@(apply #'append additional-options)
            ,@(apply #'append
                     (mapcar (lambda (arglist)
                               (seq-take arglist 2))
                             (seq-filter (lambda (arglist)
                                           (not (memq (car arglist)
                                                      `(,@options
                                                        ,@target-extensions
                                                        ,@exclusive-targets
                                                        ,@doct-keywords))))
                                         (cdr pargs))))))))

(defmacro doct (&rest args)
  "Specify Org capture templates declaratively.

The doct macro expands in one of two ways. If ARGS is an unquoted series of list
forms, doct will expand to a backquoted list of org-capture-template entries:

  (doct ((...) (...) (...)))

Expands to:

  \\=`((...) (...) (...))

If ARGS is not a series of lists, doct expands into a single
org-capture-template entry:

  (doct \"example\" :keys \"e\")

Expands to:

  (\"e\" \"example\")

This allows one to mix doct forms with the rest of their
`org-capture-templates'.


Each form must specify, at a minimum, a name and keys. The name can either be
the first value in the form or specified with the :name keyword. The :name
keyword overrides the positional argument. The :keys keyword specifies the keys
to access the template from the capture menu.

A positional name argument:

  (doct (\"example\" :keys \"e\"))

Or the :name keyword:

  (doct (\"I'm ignored and optional in this case\" :keys \"e\" :name
          \"example\"))

Both expand to:

  \\=`((\"e\" \"example\"))

Forms like these must precede forms that share a common prefix key. e.g.

  (doct (\"Templates accessed by pressing \\='e\\='\" :keys \"e\") (\"example
        one\" :keys \"eo\"...) (\"example two\" :keys \"et\"...))


The :type keyword specifies the entry's type and accepts the following symbols:

   entry An Org node with a headline. The template becomes a child of the target
               entry or a top level entry.

   item A plain list item, placed in the first plain list at the target
               location. checkitem A checkbox item. This differs from the plain
               list item only in so far as it uses a different default template.

   table-line A new line in the first table at target location.

   plain Text inserted as is.

For example:

  (doct (\"example\" :keys \"e\" :type entry ...))


The :target keyword specifies the location of the inserted template text. Using
:target directly overrides all of the other target keywords. e.g.

  (doct (... :target '(file \"/path/to/target.org\")))

The first keyword declared in the following group exclusively sets the target.
\(The :file keyword is not necessary for these)

  :id \"id of existing Org entry\" File as child of this entry, or in the body
    of the entry (see `org-id-get-create' in addition to
    `org-capture-templates')

  :clock t File to the currently clocked entry

  :function (lambda () ;visit file and move point to desired location...) This
    keyword is exclusive when used without the :file keyword. It is responsible
    for finding the proper file and location to insert the capture item. If
    :file specifies a target file, then the function is only responsible for
    moving point to the desired location within that file.

  (doct (\"example\" :keys \"e\" :clock t :function (lambda () (ignore))
         ;ignored :id \"1\" ;also ignored ...))

Expands to:

  \\=`((\"e\" \"example\" (clock)...))


The :file keyword specifies the target file for the capture template.

  (doct (... :file \"/path/to/target.org\"))

The following keywords refine the target file location:

  :headline \"node headline\" File under unique heading in target file.

  :olp \"Level 1 heading\" \"Level 2 heading\"... Specify the full outline in
    the target file. If :+datetree has a non-nil value, create a date tree for
    today's date. Use a non-nil :time-prompt property to prompt for a different
    date. Use a non-nil :tree-type property to create a week-tree.

  :regexp \"regexp describing location\" File to the entry matching regexp in
    target file

  :function function-finding-location If used in addition to the :file keyword,
    the value should be a function that finds the desired location in that file.
    If used as an exclusive keyword (see above), the function must locate both
    the target file and move point to the desired location.

The :template keyword specifies the template for creating the capture item.
Multiple strings expand into a single string joined by newlines.

  (doct (...:template \"* Test\" \"One\" \"Two\"))

Expands to:

  \\=`((...\"Test\\nOne\\nTwo\"))

The :template-file: keyword specifies a file containing the text of the
template.
The :template-function: keyword specifies a function which returns the template.
The first of these keywords found overrides any additional template
declarations.

Key Value pairs specify additional options. Doct does not include keywords with
a nil value in the expansion.

  (doct (...:immediate-finish nil))

Expands to:

  \\=`((...))

see `org-capture-templates' for a full list of additional options."

  (declare (indent 0))
  (list '\` (if (seq-every-p 'listp `,args)
                (nreverse
                 (mapcar (lambda (arg) (doct--convert-to-template-entry arg))
                         `,args))
              (doct--convert-to-template-entry args))))

(provide 'doct)

;;; doct.el ends here
