(in-package #:jack.tools.misc)

(defun empty-p (obj)
  (or (null obj) (and (stringp obj) (string= "" obj))))

(defun dekeywordfy (name) (symbol-munger:lisp->camel-case name))

(defun keywordfy (name) (values (intern (string-upcase name) "KEYWORD")))

(defun prompt-read (prompt)
  "Prompts and reads."
  (format *query-io* "~d " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-flag (flag alist &key force-string)
  (let ((str (agethash flag alist)) collect)
    (when str
      (dolist (substring (split-sequence:split-sequence #\space str))
	(cond ((string= substring "") nil)
	      (force-string (push substring collect))
	      (t (let ((new (handler-case (read-from-string substring)
			      (error () substring))))
		   (if (integerp new)
		       (push new collect)
		       (push substring collect))))))
      (car collect))))

(defmacro if-exist-return (if-part &body else-part)
  "Else-part is unsafe (side-effects via incf etc.)"
  `(let ((condition (progn ,if-part)))
     (if condition condition (progn ,@else-part))))

(defun string-alist-values (alist &key reverse)
  "Convert values in alist to string if they were not previously, or vice versa."
  (cond ((dotted-pair-p alist)
	 (cons (car alist) (string-alist-values (cdr alist) :reverse reverse)))
	((listp alist)
	 (mapcar #'(lambda (arg) (string-alist-values arg :reverse reverse))
		 alist))
	((equal "" alist) nil)
	((and reverse (stringp alist) (every #'digit-char-p alist))
	 (read-from-string alist))
	((numberp alist) (write-to-string alist))
	(alist alist)))

(defun list-package-symbols (package)
  (let (collect)
    (do-external-symbols (symbol (find-package package))
      (push symbol collect))
    (sort collect #'string>)))

(defun largest-key (table &key outputs)
  (let ((val 0) out)
    (maphash #'(lambda (key value)
		 (let ((int (if (listp value) (length value) value)))
		   (cond ((> int val) (setf out (list key) val int))
			 ((= int val) (push key out)))))
	     table)
    (if outputs
	(map-reduce #'(lambda (key) (gethash key table)) #'append out)
	out)))

(defun system-version (system-designator)
  (let ((system (asdf:find-system system-designator nil)))
    (when (and system (slot-boundp system 'asdf:version))
      (asdf:component-version system))))

(defun ql-installed-systems ()
  (mapcar #'ql::name (ql::installed-systems (ql::find-dist "quicklisp"))))
