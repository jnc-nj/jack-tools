(in-package #:jack.tools.strings)

(defun rreplace (regex string new)
  (cl-ppcre:regex-replace-all
   regex string (stringfy new)))

(defun stringfy (new)
  (cond ((stringp new) new)
	((numberp new) (write-to-string new))
	((or (characterp new) (symbolp new)) (string new))
	(t (cl-json:encode-json-to-string new))))

(defun sexpp (string)
  (handler-case (listp (read-from-string string)) (error () nil)))

(defun substringp (needle haystack &key (test #'char=))
  "Check if needle is a substring of haystack."
  (search (string needle)
	  (string haystack)
	  :test test))

(defun regexfy (lst)
  "Create a regex from a list of elements."
  (let* ((ones (remove-if-not #'(lambda (arg) (equal (length arg) 1)) lst))
	 (others (set-difference (remove-if #'(lambda (arg) (equal arg "")) lst)
				 ones :test #'equal))
	 (pointer 0))     
    (concatenate 'string
		 (when others
		   (incf pointer)
		   (format nil "~{(~d)~^|~}" others))
		 (when ones 
		   (format nil "~[~;|~][~{~d~}]" pointer ones)))))

(defun concatstring (list &key (newline 0) (splitters '(#\space)))
  "Concatenates a list of strings according to parameters."
  (string-right-trim splitters
		     (with-output-to-string (s)
		       (dolist (item (remove nil list))
			 (format s "~a~[~%~; ~;~]"
				 item newline)))))

(defun get-regex-match (str list)
  "Get sorted regex matches from str given list." 
  (sort (cl-ppcre:all-matches-as-strings list str)
	#'> :key #'length))

(defun string-test-p (content &key (test #'numberp))
  (let ((unstring (handler-case (read-from-string content) (error () nil))))
    (cond ((and unstring (listp unstring)) (every test unstring))
	  (unstring (funcall test unstring)))))

(defun trim-whitespace (str &key exclude)
  (trim-string str :chars (list #\space #\tab #\newline) :exclude exclude))

(defun trim-string (str &key chars exclude)
  (if (listp str)
      (mapcar #'(lambda (arg) (string-trim arg :chars chars :exclude exclude)) str)
      (string-trim (remove-if #'(lambda (arg) (member arg exclude)) chars)
		   str)))

(defun brace-balance-p (str)
  (when (stringp str)
    (let ((l (count "(" str :test #'string=))
	  (r (count ")" str :test #'string=)))
      (values (= l r) l r))))

(defun perfect-match (regex str)
  (multiple-value-bind (start end) (cl-ppcre:scan regex str)
    (and (= start 0) (= end (length str)))))
