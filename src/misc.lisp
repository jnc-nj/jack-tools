(in-package #:jack.tools.misc)

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
  `(if ,if-part ,if-part (progn ,@else-part)))

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
