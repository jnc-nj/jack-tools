(in-package :jack.tools.serialize)

(defclass serial-dict ()
  ((n2s :initarg :n2s :initform (make-hash-table :test #'equal))
   (s2n :initarg :s2n :initform (make-hash-table :test #'equal))
   (ix :initarg :ix :initform -1)))

(defclass serial-object ()
  ((var-dict :initarg :var-dict :initform (make-instance 'serial-dict))
   (fn-dict :initarg :fn-dict :initform (make-instance 'serial-dict))
   (sexp-dict :initarg :sexp-dict :initform '())))

(defvar *serial* (make-instance 'serial-object))

(defmethod n2s-of ((serial-dict serial-dict))
  (with-slots (n2s) serial-dict n2s))

(defmethod s2n-of ((serial-dict serial-dict))
  (with-slots (s2n) serial-dict s2n))

(defmethod ix-of ((serial-dict serial-dict))
  (with-slots (ix) serial-dict ix))

(defmethod serial-update (item (serial-dict serial-dict))
  (with-slots (ix) serial-dict
    (cond ((listp item) (mapcar #'(lambda (itm) (serial-update itm serial-dict)) item))
	  ((numberp item) item)
	  ((gethash item (n2s-of serial-dict)) item)
	  ((gethash item (s2n-of serial-dict))
	   (gethash item (s2n-of serial-dict)))
	  (t (setf (gethash (incf ix) (n2s-of serial-dict)) item
		   (gethash item (s2n-of serial-dict)) ix)))))

(defmethod serial-read (item (serial-dict serial-dict))
  (if (numberp item)
      (gethash item (n2s-of serial-dict))
      (gethash item (s2n-of serial-dict))))

(defmethod serial-output ((serial-object serial-object))
  (let ((sexp-table (make-hash-table :test #'equal)))
    (with-slots (var-dict fn-dict sexp-dict) serial-object
      (dolist (item sexp-dict)
	(setf (gethash (serial-update (first item) var-dict) sexp-table)
	      `(,(second item) ,@(loop for itm in (cddr item) collect
				      (serial-update itm var-dict)))))
      (make-instance
       'serial-object
       :var-dict (s2n-of var-dict)
       :fn-dict (s2n-of fn-dict)
       :sexp-dict sexp-table))))

(defun serial (objects)
  (setf *serial* (make-instance 'serial-object))
  (with-slots (sexp-dict) *serial*
    (dolist (object objects)
      (let ((head (first object))
	    (form (serialize (second object)))
	    (rest (cddr object)))
	(push `(,head ,form ,@rest) sexp-dict)))
    (serial-output *serial*)))

(defun serialize (object)
  (with-slots (var-dict fn-dict) *serial*
    (cond ((listp object)
	   (reverse
	    (cons (serial-update (car object) fn-dict)
		  (mapcar #'serialize (cdr object)))))
	  (object
	   (serial-update object var-dict)))))
