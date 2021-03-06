(in-package #:jack.tools.objects)

(defclass address-book ()
  ((interfaces :initarg :interfaces :initform nil)
   (stack :initarg :stack :initform nil)))

(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT with optional reinitialization.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
	   (copy (allocate-instance class)))
      (dolist (slot-name (get-slot-names class))
	(when (slot-boundp object slot-name)
	  (setf (slot-value copy slot-name) (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

(defun to-address-book (object)
  (let ((output-object (make-instance 'address-book)))
    (with-slots (interfaces stack) output-object
      (labels ((interpret (object) 
		 (cond ((alistp object) object)
		       ((listp object) (mapcar #'interpret object)) 
		       ((objectp object)
			(let* ((class (class-of object))
			       (object-id (get-object-id object))
			       (slot-names (get-slot-names class))
			       switch) 
			  (dolist (slot-name slot-names)
			    (when (slot-boundp object slot-name)
			      (let* ((slot-value (slot-value object slot-name))
				     (interpreted (interpret (slot-value object slot-name))))
				(unless (or switch (equal slot-value interpreted))
				  (setf switch t))
				(setf (slot-value object slot-name) interpreted))))
			  (if switch
			      (pushnew (cons object-id object) interfaces
				       :test #'slots-equal? :key #'cdr)
			      (pushnew (cons object-id object) stack
				       :test #'slots-equal? :key #'cdr))
			  object-id))
		       (t object))))
	(interpret object)
	output-object))))

(defun cast (alist class-map &key strict)
  (when (alistp alist)
    (let* ((input-class (find-class-map (mapcar #'car alist) class-map :strict strict))
	   (instance (when input-class (make-instance input-class)))
	   (class (class-of instance))) 
      (when instance 
	(dolist (slot-name (get-slot-names class))
	  (let ((value (agethash slot-name alist)))
	    (setf (slot-value instance slot-name)
		  (cond ((alistp value) (cast value class-map :strict strict))
			((or (not (listp value)) (not (listp (car value)))) value)
			((listp (caar value))
			 (mapcar #'(lambda (v) (cast v class-map :strict strict)) value))
			(t value))))) 
	(return-from cast instance))
      alist))
  alist)

(defun cast-all (objects class-map)
  (loop for object in objects
       collect (cast object class-map)))

(defun create-class-map (&rest classes)
  (let ((collect (make-hash-table :test #'equal)))
    (dolist (class classes)
      (let ((temp-object (make-instance class)))
	(eval (generate-json-method class))
	(setf (gethash class collect)
	      (get-slot-names (class-of temp-object)))))
    collect))

(defun find-class-map (alist-names class-map &key strict) 
  (let ((intersect-length 0) faux-key subset?)
    ;; if alist-names is equivalent to value, return
    ;; if alist-names is a subset of value, return largest value
    ;; if alist-names is an intersection of value, return largest intersection
    ;; else return nothing
    (maphash
     #'(lambda (key value)
	 (let ((equals (set-equals alist-names value :test #'string= :key #'string-upcase))
	       (subset (unless strict (subsetp alist-names value :test #'string= :key #'string-upcase)))
	       (intersect (unless strict (intersection alist-names value :test #'string= :key #'string-upcase))))
	   (cond (equals (return-from find-class-map key)) 
		 ((and (not strict)
		       (or (and (not subset?) subset)
			   (and subset (> (length value) (length (gethash faux-key class-map))))))
		  (setf faux-key key subset? t))
		 ((and (not strict) intersect (not subset?) (> (length intersect) intersect-length))
		  (setf faux-key key intersect-length (length intersect))))))
     class-map)
    faux-key))

(defun get-class-id (class)
  (car (cl-ppcre:all-matches-as-strings
	"(?<=:)(.+?)(?=>)"
	(write-to-string class))))

(defun get-object-id (object)
  (car (cl-ppcre:all-matches-as-strings
	"(?<={)(.+?)(?=})"
	(write-to-string object))))

(defun get-slot-names (class)
  (mapcar #'c2mop:slot-definition-name
	  (c2mop:class-slots class)))

(defun slots-equal? (object-1 object-2)
  (let ((class-1 (class-of object-1))
        (class-2 (class-of object-2)))
    (when (eq class-1 class-2)
      (dolist (slot-name (get-slot-names class-1))
        (unless (equal (slot-value object-1 slot-name)
                       (slot-value object-2 slot-name))
          (return-from slots-equal? nil)))
      (return-from slots-equal? t))
    (equal object-1 object-2)))

(defun objectp (object)
  (handler-case (make-instance (class-of object))
    (error () nil)))

(defun object-to-alist (object)
  (cond ((stringp object) (cl-json:decode-json-from-string object))
	((alistp object) object)
	(t (object-to-alist (jonathan:to-json object)))))

(defun generate-json-method (class-name)
  (let* ((slot-names (get-slot-names (class-of (make-instance class-name)))))
    `(defmethod %to-json ((,class-name ,class-name))
       (with-slots ,slot-names ,class-name
	 (with-object
	   ,@(loop for item in slot-names collect
		  `(write-key-value
		    ,(dekeywordfy item)
		    (if (alistp ,item)
		        (recursive-alist-hash-table ,item)
			,item))))))))

(defun get-object-size (object)
  (sb-sys:without-gcing
    (/ (nth-value 2 (sb-vm::reconstitute-object
		     (ash (logandc1 sb-vm:lowtag-mask
				    (sb-kernel:get-lisp-obj-address object))
			  (- sb-vm:n-fixnum-tag-bits))))
       8)))

(defun *slot-value (object slot-name)
  (handler-case (slot-value object slot-name)
    (error () nil)))
