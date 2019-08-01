(in-package #:jack.tools.lists)

(defun agethash (keyword alist &key (test #'string=) (key #'string-upcase))
  (when (alistp alist)
    (cdr (assoc (funcall key (string keyword))
		alist :test test :key key))))

(defun agethash-vals (keyword alist &key (result-type 'vector) (test #'string=) (key #'string-upcase))
  (map result-type #'(lambda (arg) (agethash keyword arg :test test :key key))
       alist))

(defun split-list (lst)
  (delete-if #'null
	     (loop for item in lst
		for idx from 0 collect
		  (when (evenp idx)
		    (cons item (nth (+ idx 1) lst))))
	     :key #'cdr))

(defun combinations (&rest lsts)
  "Get all combinations cross lists.
   For single list: (combinations lst lst)"
  (let ((lists (remove nil lsts)))
    (if (car lists)
	(mapcan #'(lambda (inner-val)
		    (mapcar #'(lambda (outer-val)
				(cons outer-val inner-val))
			    (car lists)))
		(apply #'combinations (cdr lists)))
	(list nil))))

(defun alistp (lst)
  (and (listp lst) (every #'listp lst)))

(defun trim-seq (seq start &optional end)
  (cond ((null end) (subseq seq start end))
	((> start (length seq)) seq)
	((> end (length seq)) (subseq seq start))
	(t (subseq seq start end))))

(defun trim-sort (key seq limit direction &key (start 0))
  (trim-seq (sort seq direction :key key) start limit))

(defun union-sort (key seq-1 seq-2 direction)
  (sort (union seq-1 seq-2 :key key) direction :key key))

(defun all-positions (object lst &key (test 'equal))
  (let (positions)
    (loop for item in lst
       for index from 0
       do (when (funcall test object item)
	    (push index positions)))
    positions))

(defun map-reduce (map-fn reduce-fn objects &key reduce-key)
  (reduce reduce-fn (mapcar map-fn objects) :key reduce-key))

(defun window (window object lst &key (test 'equal))
  "Get all elements within window N of OBJECT in LST."
  (let ((lst-length (length lst)))
    (reduce #'append
	    (loop for position in (all-positions object lst :test test)
                  for lower-bound = (- position window)
                  for upper-bound = (+ position window 1)
                  collect (delete object
                                  (subseq lst
                                          (if (> lower-bound 0) lower-bound 0)
                                          (when (< upper-bound lst-length) upper-bound))
                                  :test test)))))

(defun random-item (lst &key value)
  (cond ((listp lst) (nth (random (length lst)) lst)) 
        ((and value (hash-table-p lst)) (gethash (random-item (hash-table-keys lst)) lst))
        ((hash-table-p lst) (random-item (hash-table-keys lst)))))


(defun random-selection (lst &key (limit 5) value)
  (cond ((listp lst)
         (trim-seq (loop repeat (+ 1 (random (- (length lst) 1)))
                      collect (random-item lst))
                   0 limit))
        ((hash-table-p lst)
         (trim-seq (if value
                       (mapcar #'(lambda (item) (gethash item lst))
                               (random-selection (hash-table-keys lst)))
                       (random-selection (hash-table-keys lst)))
                   0 limit))))

(defun set-equals (lst-1 lst-2 &key (test #'equal) key)
  (null (set-exclusive-or lst-1 lst-2 :key key :test test)))

(defun dotted-pair-p (obj)
  (and (listp obj)
       (not (listp (cdr obj)))))

(defun every-list-p (obj &key not)
  (when (listp obj)
    (if not
      (every #'(lambda (arg) (not (listp arg))) obj)
      (every #'listp obj))))

(defun alist-to-plist (alist)
  (let (collect)
    (dolist (item alist)
      (if (keywordp (car item))
	  (push (car item) collect)
	  (push (read-from-string (format nil ":~d" (car item))) collect))
      (push (cdr item) collect))
    (reverse collect)))
