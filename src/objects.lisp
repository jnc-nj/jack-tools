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
			       (class-id (get-class-id class))
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

(defun cast (alist class-map)
  (when (listp alist)
    (let* ((input-class (find-class-map (mapcar #'car alist) class-map))
           (instance (when input-class (make-instance input-class)))
           (class (class-of instance))) 
      (when instance 
        (dolist (slot-name (get-slot-names class))
          (let ((value (agethash slot-name alist)))
            (setf (slot-value instance slot-name)
                  (cond ((alistp value) (cast value class-map))
                        ((or (not (listp value)) (not (listp (car value)))) value)
                        ((listp (caar value)) (mapcar #'(lambda (v) (cast v class-map)) value))
                        (t value))))) 
        (return-from cast instance))
      alist))
  alist)

(defun create-class-map (&rest classes)
  (let ((collect (make-hash-table :test #'equal)))
    (dolist (class classes)
      (let ((temp-object (make-instance class)))
	(eval (generate-json-method class))
	(setf (gethash class collect)
	      (get-slot-names (class-of temp-object)))))
    collect))

(defun find-class-map (alist-names class-map) 
  (maphash #'(lambda (key value) 
	       (when (null (set-exclusive-or alist-names value :test #'string=))
		 (return-from find-class-map key)))
	   class-map))

(defun get-class-id (class)
  (car (cl-ppcre:all-matches-as-strings
	"(?<=:)(.+?)(?=>)"
	(write-to-string class))))

(defun get-object-id (object)
  (car (cl-ppcre:all-matches-as-strings
	"(?<={)(.+?)(?=})"
	(write-to-string object))))

(defun get-slot-names (class)
  (mapcar #'sb-mop:slot-definition-name
	  (sb-mop:class-slots class)))

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
  (not (or (symbolp object)
	   (numberp object)
	   (stringp object)
	   (listp object)
	   (hash-table-p object)
	   (eq t object)
	   (eq nil object))))

(defun object-to-alist (object)
  (cl-json:decode-json-from-string
   (cl-json:encode-json-to-string object)))

(defun generate-json-method (class-name)
  (let* ((slot-names (get-slot-names (class-of (make-instance class-name)))))
    `(defmethod %to-json ((,class-name ,class-name))
       (with-slots ,slot-names ,class-name
	 (with-object
	   ,@(loop for item in slot-names collect
		  `(write-key-value ,(read-from-string (cl-json:encode-json-to-string item))
				    (if (alistp ,item)
					(with-object
					  (dolist (i ,item)
					    (log:info i)
					    (write-key (dekeywordfy (car i)))
					    (write-value (cdr i))))
					,item))))))))
