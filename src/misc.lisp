(in-package #:jack.tools.misc)

(defun keywordfy (name) (values (intern (string-upcase name) "KEYWORD")))

(defun prompt-read (prompt)
  "Prompts and reads."
  (format *query-io* "~d " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-flag (flag alist)
  (let ((str (agethash flag alist)))
    (when str
      (car (mapcar #'(lambda (arg)
		       (if (string= arg "") arg
			   (let ((new (read-from-string arg)))
			     (if (integerp new) new str))))
		   (split-sequence:split-sequence #\space str))))))

(defun count-threads (key)
  "Count threads matching key."
  (let ((count 0))
    (dolist (thread (sb-thread:list-all-threads))
      (when (search key (sb-thread:thread-name thread))
	(incf count)))
    count))

(defun decode-http-body (body)
  "For drakma."
  (babel:octets-to-string
   (coerce body '(vector (unsigned-byte 8)))))
