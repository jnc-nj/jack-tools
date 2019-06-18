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
	      (t (let ((new (read-from-string substring)))
		   (if (integerp new)
		       (push new collect)
		       (push substring collect))))))
      (car collect))))

(defun jsonp (str)
  (eq (char str 0) #\{))

(defun decode-http-body (body &key (json? t))
  (cond ((and (stringp body) (jsonp body) json?)
	 (cl-json:decode-json-from-string body))
	((or (stringp body) (numberp body)) body)
	(t (decode-http-body
	    (babel:octets-to-string
	     (coerce body '(vector (unsigned-byte 8))))))))

(defun encode-http-body (body)
  (cond ((stringp body) body)
	((alistp body) (jonathan:to-json body :from :alist))
	(t (jonathan:to-json body))))

(defun if-exist-return (if-part else-part)
  "Else-part is unsafe (side-effects via incf etc.)"
  (if if-part if-part else-part))

(defun get-all-symbols (&optional package)
  (let ((lst ())
        (package (find-package package)))
    (do-all-symbols (s lst)
      (when (fboundp s)
        (if package
            (when (eql (symbol-package s) package)
              (push s lst))
            (push s lst))))
    lst))

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

(defmacro defhandler ((app uri &key (method :get) (content-type "application/json")) &body body)
  `(setf (ningle:route ,app ,uri :method ,method)
	 #'(lambda (params)
	     (declare (ignorable params))
	     (setf (getf (response-headers ningle:*response*)
			 :content-type)
		   ,content-type)
	     (let ((http-content* (decode-http-body (request-content ningle:*request*))))
	       (encode-http-body (progn ,@body))))))
