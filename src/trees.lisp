(in-package #:jack.tools.trees)

(defun tree-similarity (tree-1 tree-2) 
  (cos-similarity (upsilon tree-1) (upsilon tree-2)))

(defun upsilon (tree)
  "Upsilion of tree."
  (cond ((null tree) nil)
        ((atom tree) (list 0))
        (t (cons (length tree)
                 (reduce #'append
			 (sort (mapcar #'upsilon tree) 
			       #'compare-trees))))))
(defun compare-trees (a b)
  "Compare tree a to tree b."
  (cond ((and (atom a) (atom b)) t)
        ((atom a) t)
        ((atom b) nil)
        ((equal (car a) (car b)) (compare-trees (cdr a) (cdr b)))
        (t (compare-trees (car a) (car b)))))

(defun eliminate (item list)
  "Recusrively eliminate item from list."
  (delete nil (mapcar #'(lambda (arg)
			  (unless (equal item arg)
			    (if (listp arg)
				(eliminate item arg)
				arg)))
		      list)))

(defun subber (lst sublist &key read?)
  (if (listp lst)
      (loop for item in lst
	 collect (subber item sublist :read? read?))
      (let ((found (agethash lst sublist)))
	(cond ((and read? found) (read-from-string found))
	      (found found)
	      (t lst)))))

(defun unquote (lst &key quote?)
  "Tail-recursive unquotes all elements in lst."
  (if (listp lst)
      (mapcar #'(lambda (arg)
		  (cond ((listp arg) (unquote arg :quote? quote?))
			((stringp arg) (read-from-string arg))
			((symbolp arg) (if quote? (string arg) arg))
			(t arg)))
	      lst)
      (if quote? (string lst) lst)))

(defun recursive-alist-hash-table (alist hashtable)
  (cond ((not (listp alist)) alist)
	((alistp (car alist))
	 (loop for arg in alist collect
	      (recursive-alist-hash-table arg (make-hash-table :test #'equal))))
	((alistp alist)
	 (dolist (arg alist)
	   (recursive-alist-hash-table arg hashtable))
	 hashtable) 
	((listp alist)
	 (setf (gethash (car alist) hashtable)
	       (handler-case
		   (if (member t (mapcar #'(lambda (arg) (alistp arg)) (cdr alist)))
		       (recursive-alist-hash-table (cdr alist)
						   (make-hash-table :test #'equal))
		       (cdr alist))
		 (type-error () (recursive-alist-hash-table (cdr alist)
							    (make-hash-table :test #'equal)))))
	 hashtable)
	(t alist)))
