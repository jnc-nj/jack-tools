(in-package #:jack.tools.misc)

(defun dekeywordfy (name) (read-from-string (cl-json:encode-json-to-string name)))

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

(defun count-threads (key)
  "Count threads matching key."
  (let ((count 0))
    (dolist (thread (sb-thread:list-all-threads))
      (when (search key (sb-thread:thread-name thread))
	(incf count)))
    count))

(defun decode-http-body (body)
  "For drakma."
  (if (stringp body) body
      (babel:octets-to-string
       (coerce body '(vector (unsigned-byte 8))))))

(defun if-exist-return (if-part else-part)
  "Else-part is unsafe (side-effects via incf etc.)"
  (if if-part if-part else-part))

(defun create-directory (name)
  (ensure-directories-exist (pathname name)))

(defun write-file (object target)
  (with-open-file (file (pathname target)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (if (stringp object)
        (format file "~d" object)
        (format file "~d" (cl-json:encode-json-to-string object)))))

(defun open-file (target)
  (with-open-file (file (pathname target))
    (let ((contents (make-string (file-length file))))
      (read-sequence contents file)
      contents)))

(defun cmd-read-path (path)
  (format nil "$(<~d)" (pathname path)))

(defun *probe-file (path)
  (when path (probe-file path)))

(defun join-thread (thread-name)
  (sb-thread:join-thread
   (find-if #'(lambda (thread) (search thread-name (sb-thread:thread-name thread)))
            (sb-thread:list-all-threads))))

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
