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

