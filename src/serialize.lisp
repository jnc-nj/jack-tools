(in-package :jack.tools.serialize)

(defvar *serial* (make-instance 'serial-object))

(defclass serial-dict ()
  ((n2s :initarg :n2s :initform (make-hash-table :test #'equal))
   (s2n :initarg :s2n :initform (make-hash-table :test #'equal))
   (ix :initarg :ix :initform -1)))

(defclass serial-object ()
  ((var-dict :initarg :var-dict :initform (make-instance 'serial-dict))
   (fn-dict :initarg :fn-dict :initform (make-instance 'serial-dict))
   (end-dict :initarg :end-dict :initform (make-instance 'serial-dict))
   (sexp-dict :initarg :sexp-dict :initform '())))

(defmethod n2s-of ((serial-dict serial-dict))
  (with-slots (n2s) serial-dict n2s))

(defmethod s2n-of ((serial-dict serial-dict))
  (with-slots (s2n) serial-dict s2n))

(defmethod ix-of ((serial-dict serial-dict))
  (with-slots (ix) serial-dict ix))

(defmethod serial-update (item (serial-dict serial-dict))
  (with-slots (ix) serial-dict
    (if (gethash item (s2n-of serial-dict))
	(gethash item (s2n-of serial-dict))
	(setf (gethash (incf ix) (n2s-of serial-dict)) item
	      (gethash item (s2n-of serial-dict)) ix))))

(defmethod serial-read (item (serial-dict serial-dict))
  (if (numberp item)
      (gethash item (n2s-of serial-dict))
      (gethash item (s2n-of serial-dict))))

(defmethod serial-output ((serial-object serial-object))
  (with-slots (var-dict fn-dict end-dict) serial-object
    (make-instance
     'serial-object
     :var-dict (s2n-of var-dict)
     :fn-dict (s2n-of fn-dict)
     :end-dict (s2n-of end-dict)
     :sexp-dict
     (alist-hash-table
      (loop for item in sexp-dict
	 for index = (serial-read (car item) var-dict) collect
	   (if index
	       (cons index (cdr item))
	       (cons (serial-update (car-item) end-dict) (cdr item))))))))

(defun serial (objects)
  (setf *serial* (make-instance 'serial-object))
  (with-slots (sexp-dict) *serial*
    (dolist (object objects)
      (push (cons (car object) (serialize (cdr object))) sexp-dict))
    *serial*))

(defun serialize (object)
  (with-slots (var-dict fn-dict) *serial*
    (cond ((listp object)
	   (cons (serial-update (car object) fn-dict)
		 (mapcar #'serialize (cdr object))))
	  (object
	   (serial-update object var-dict)))))
