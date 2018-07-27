(in-package #:jack.tools.lists)

(defun agethash (key alist &key (test 'string=))
  (cdr (assoc key alist :test test)))

(defun agethash-vals (key alist &key (result-type 'vector) (test 'string=))
  (map result-type #'(lambda (arg) (agethash key arg :test test))
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

(defun alistp (obj)
  (when (and (listp obj)
	     (listp (car obj))
	     (not (listp (caar obj))))
    t))

(defun trim-seq (seq start &optional end)
  (cond ((null end) (subseq seq start end))
	((> start (length seq)) seq)
	((> end (length seq)) (subseq seq start))
	(t (subseq seq start end))))

(defun trim-sort (key seq limit direction &key (start 0))
  (trim-seq (sort seq direction :key key) start limit))

(defun all-positions (object lst &key (test 'equal))
  (let (positions)
    (loop for item in lst
       for index from 0
       do (when (funcall test object item)
	    (push index positions)))
    positions))

(defun map-reduce (map-fn reduce-fn objects
                   &key map-key reduce-key)
  (reduce reduce-fn (mapcar map-fn objects :key map-key)
          :key reduce-key))

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
