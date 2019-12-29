(defun doct--map-plist (fn plist)
  "Map binary function FN over PLIST."
  (let ((p plist)
        mapped)
    (while p
      (push (funcall fn (car p) (cadr p)) mapped)
      (setq p (cddr p)))
    (nreverse mapped)))

(defun doct--translate-target (target)
  "Translate `org-capture-plist's :target to doct syntax."
  (pcase target
    (`(file ,path) `(:file ,path))
    (`(file+headline ,path ,headline) `(:file ,path :headline ,headline))
    (`(file+olp . (,path . ,olp)) `(:file ,path :olp ,olp))
    (`(file+olp+datetree . (,path . ,olp)) `(:file ,path :datetree t :olp ,olp))
    (`(file+regexp ,path ,regexp) `(:file ,path :regexp ,regexp))
    (`(file+function ,path ,fn) `(:file ,path :function ,fn))
    (`(id ,id) `(:id ,id))
    (`(clock) `(:clock t))
    (`(function ,fn) `(:function ,fn))))

(defun doct--translate-template (template)
  "Translate `org-capture-plist's :template to doct syntax."
  (pcase template
    (`(file ,path) `(:template-file ,path))
    (`(function ,fn) `(:template-function ,fn))
    (_ (if (string-match "\n" template)
           `(:template ,(split-string template "\n"))
         `(:template ,template)))))

(defun doct--translate-pair (key val)
  "Translate Org capture syntax to doct syntax for KEY VAL pair."
  (pcase `(,key ,val)
    (`(:key ,key) `(:keys ,key))
    (`(:description ,name) `(:name ,name))
    (`(:target ,target) `(,@(doct--translate-target target)))
    (`(:template ,template) `(,@(doct--translate-template template)))
    (_ `(,key ,val))))

(defun doct--upgrade-plist-to-form (plist)
  "Return a FORM that doct will accept as an argument."
  ;;("name" &rest properties)
  `(,(plist-get plist :name) ,@(org-plist-delete plist :name)))

(defun doct--upgrade-template (template)
  (let ((org-capture-plist '()))
    (org-capture-set-plist template)
    (doct--upgrade-plist-to-form
     (apply 'append
            (doct--map-plist 'doct--translate-pair
                             org-capture-plist)))))

;; (defun doct--upgrade-families (forms)
;;   "Returns a list of (PARENT (CHILDREN)) from FORMS."
;;   (let (families)
;;   (dolist (form forms)
;;     ())))

(defun doct--upgrade-templates (templates)
  "Translate each template in TEMPLATES to doct syntax."
  (mapcar 'doct--upgrade-template templates))

;;TESTS
(defun get-templates (file-path)
  ""
  (eval (read
         (with-temp-buffer
           (insert-file-contents file-path)
           (buffer-string)))))

(defun valid-templates (file-path)
  (let ((org-capture-templates (get-templates file-path)))
    (org-capture)))

(let* ((org-directory "./tests/captures")
       (test-files '("pashinin.el" "spacebat.el" "massimo-lauria.el"))
       (test-file (nth 2 test-files))
       (validate nil)
       (test-path (concat "./tests/templates/" test-file)))
  (if validate
      (valid-templates test-path)
    (doct (doct--upgrade-templates (get-templates test-path)))))

(let ((org-capture-templates (doct '(("variable" :keys "v"
                                :file org-default-notes-file
                                :headline "test"))))))
  (org-capture))
