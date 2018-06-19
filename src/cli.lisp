(in-package #:jack.tools.cli)

(defclass menu ()
  ((name :initarg :name :initform "top-level") 
   (prompt :initarg :prompt :initform ">>")
   (form :initarg :form :initform "::~{~:@(~a~)::~}~%")
   (links :initarg :links :initform '())
   (functions :initarg :functions :initform '())
   (run :initarg :run :initform '())
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

(defmethod get-sub-menu (sub-menus (menu menu))
  (let ((temp-menu menu))
    (with-slots (functions links) temp-menu 
      (if sub-menus
	  (loop for sub-menu = (pop sub-menus)
	     for temp-sub-menu = (find sub-menu links :key #'name-of :test #'string=)
	     do (when temp-sub-menu (setf temp-menu temp-sub-menu))
	     while sub-menus
	     finally (return temp-sub-menu))
	  temp-menu))))

(defmethod add-sub-menu (sub-menu (menu menu) &key sub-menus)
  (with-slots (links) (get-sub-menu sub-menus menu)
    (push sub-menu links)))

(defmethod add-run (function (menu menu) &key sub-menus)
  (with-slots (run) (get-sub-menu sub-menus menu)
    (push function run)))

(defmethod add-function (name function (menu menu) &key sub-menus)
  (with-slots (functions) (get-sub-menu sub-menus menu)
    (push (cons name function) functions)))

(defmethod with-cli ((menu menu))
  (loop do (with-slots (name prompt form links functions run exit) menu
	     (funcall run)
	     (format *standard-output* "[~:@(~a~)]" name)
	     (format *standard-output* form (append (list exit) (mapcar #'name-of links))) 
	     (let* ((input (string-downcase (prompt-read prompt)))
		    (link (next-link input menu))
		    (function (agethash input functions)))
	       (cond ((string= input exit) (return-from with-cli))
		     (link (with-cli link))
		     (function (funcall function))
		     (t (format *standard-output* "~d UNK '~:@(~a~)'~%" prompt input)))))))
