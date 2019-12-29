(defmacro doct-capture-test (template &rest body)
  (declare (indent defun))
  `(let ((org-capture-templates ,template))
     ,@body
     (debug)
     (org-capture)))

(doct-capture-test
  (doct '(("test"
           :type plain
           :keys "t"
           :file "doesnt-exist"
           :template "* OK %?")))
  (setq org-directory "~/.emacs.d/lisp/doct/tests/captures/"))
