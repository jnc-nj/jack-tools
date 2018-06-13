(in-package #:jack.tools.cli)

(defclass menu ()
  ((name :initarg :name :initform "top-level")
   (prompt :initarg :prompt :initform ">>")
   (format :initarg :format :initform "::~{~:@(~a~)::~}~%")
   (links :initarg :links :initform '())
   (functions :initarg :functions :initform '())
   (exit :initarg :exit :initform "back")))

(defun load-menu (path)
  (cast (cl-json:decode-json-from-source (pathname path))
        (create-class-map 'menu)))

(defmethod name-of ((menu menu))
  (with-slots (name) menu name))

(defmethod next-link (name (menu menu))
  (with-slots (links) menu
    (find name links
	  :key #'name-of
	  :test #'string=)))

(defmethod with-cli ((menu menu))
  (handler-case
      (loop do (with-slots (name prompt format links functions exit) menu
		 (format *standard-output* "[~:@(~a~)]~%" name)
		 (format *standard-output* format (append (list exit) (mapcar #'name-of links))) 
		 (let* ((input (string-downcase (prompt-read prompt)))
			(link (next-link input menu))
			(function (agethash input functions)))
		   (cond ((string= input exit) (return-from with-cli))
			 (link (with-cli link))
			 (function (funcall function))
			 (t (format *standard-output* "~d UNK '~:@(~a~)'~%" prompt input))))))
    (sb-sys:interactive-interrupt () (sb-ext:exit))))
