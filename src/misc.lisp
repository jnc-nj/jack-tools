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
  (if (stringp body) body
      (babel:octets-to-string
       (coerce body '(vector (unsigned-byte 8))))))

(defun if-exist-return (if-part else-part)
  (if if-part if-part else-part))

(defun create-directory (name &key (head "./"))
  (ensure-directories-exist (pathname (format nil "~d~d" head name))))

(defun write-file (object target)
  (with-open-file (file (pathname target)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (if (stringp object)
        (format file object)
        (format file (cl-json:encode-json-to-string object)))))

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
