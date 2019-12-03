(in-package #:jack.tools.couchdb)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +couch-host+ nil)
  (defvar +couch-port+ nil)
  (defvar +couch-user+ nil)
  (defvar +couch-key+ nil))

(defmacro with-couch (name &body body)
  "FFS the clouchdb package doesn't have a proper with macro."
  `(let ((*couchdb* (make-db :name ,name
			     :host ,+couch-host+
			     :port ,+couch-port+
			     :user ,+couch-user+
			     :password ,+couch-key+)))
     (ignore-errors ,@body)))

(defmacro get-ids (database)
  `(with-couch ,database
     (query-document '(:** :|id|) (get-all-documents))))

(defmacro return-document (id database &key class-map)
  `(with-couch ,database
     (let ((document (get-document ,id :if-missing :ignore)))
       (when document
	 (if ,class-map
	     (cast (cddr document) ,class-map)
	     document)))))

(defmacro return-view (view database &key class-map)
  `(with-couch ,database
     (let ((items (ad-hoc-view ,view)))
       (when items
	 (if ,class-map
	     (cast-all items ,class-map)
	     items)))))

(defmacro add-doc (database object id)
  `(with-couch ,database
     (let ((document (get-document ,id :if-missing :ignore)))
       (if document
	   (put-document (append (list (assoc :|_id| document)
				       (assoc :|_rev| document))
				 (object-to-alist ,object)))
	   (put-document (object-to-alist ,object) :id ,id)))))

(defmacro delete-doc (database id)
  `(with-couch ,database
     (let ((doc (get-document ,id :if-missing :ignore)))
       (when doc
	 (delete-document doc :if-missing :ignore)))))

(defun restart-db (db)
  (dolist (id (get-ids db))
    (delete-doc db id)))
