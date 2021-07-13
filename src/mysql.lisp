(in-package :jack.tools.mysql)

(defvar *mysql-host* nil)
(defvar *mysql-port* nil)
(defvar *mysql-user* nil)
(defvar *mysql-key* nil)
(defvar *mysql-main-db* nil)

(defun reconnect ()
  (ignore-errors (disconnect))
  (connect :host *mysql-host*
	   :port *mysql-port*
	   :user *mysql-user*
	   :password *mysql-key*
	   :database *mysql-main-db*
	   :max-connections 100)
  (query "SET character_set_connection=utf8; SET character_set_results=utf8; SET character_set_client=utf8;"))

(defun insert (db dump &key (default-time-columns t) batch)
  (let ((time-now (unless batch (create-time)))
	(_dump (cond ((and batch (every #'alistp dump)) dump)
		     (batch (query-result-to-alist dump))
		     (t (remove (assoc "id" dump :test #'string=)
				dump
				:test #'equal)))))
    (cond (batch
	   (query (format nil "INSERT INTO `~d` (~{~d~^,~}) VALUES ~{(~{'~d'~^,~})~^,~} ON DUPLICATE KEY UPDATE id=id"
			  db
			  (mapcar #'car (car _dump))
			  (loop for item in _dump collect (mapcar #'cdr item)))))
	  (default-time-columns
	   (query (format nil "INSERT INTO `~d` (~{~d~^,~},create_time,update_time) VALUES (~{'~d'~^,~},'~d','~d') ON DUPLICATE KEY UPDATE id=id"
			  db
			  (mapcar #'car _dump)
			  (mapcar #'cdr _dump)
			  time-now
			  time-now)))
	  (t (query (format nil "INSERT INTO `~d` (~{~d~^,~}) VALUES (~{'~d'~^,~}) ON DUPLICATE KEY UPDATE id=id"
			    db
			    (mapcar #'car _dump)
			    (mapcar #'cdr _dump)))))))

(defun update (db dump &key (update-key "id") batch query-string)
  (cond (batch
	 (let* ((transactions (loop for item in dump
				    collect (update db item
						    :update-key update-key
						    :query-string t)))
		(batch-query (format nil "START TRANSACTION;~{~d~^;~};COMMIT;" transactions)))
	   (query batch-query)))
	(query-string
	 (format nil "UPDATE `~d` SET update_time='~d',~{~d='~d'~^,~} WHERE ~d='~d'"
		 db
		 (create-time)
		 (alist-plist (remove (assoc "id" dump :test #'string=)
				      dump
				      :test #'equal))
		 update-key
		 (agethash update-key dump)))
	(t
	 (query (update db dump
			:update-key update-key
			:query-string t)))))

(defun select-by-params (db dump)
  (query (format nil "SELECT * FROM `~d` WHERE ~{~d='~d' ~^AND ~}"
		 db (alist-plist (remove (assoc "id" dump :test #'string=)
					 dump
					 :test #'equal)))))

(defun select-by-columns (db &rest columns)
  (query (format nil "SELECT ~{~d~^,~} FROM `~d`" columns db)))

(defun query-result-to-alist (results &key no-id)
  (destructuring-bind (rows columns) (car results)
    (let (collect)
      (dolist (row rows)
        (push (remove nil
		      (loop for item in row
			    for column in columns
			    collect (when (and item
					       (not (and no-id
							 (string= "id" (car column)))))
				      (cons (car column) item))))
	      collect))
      collect)))
