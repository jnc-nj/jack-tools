(in-package #:jack.tools.maths)

(defun random-float (min max &key (seed 2048))
  "Calculate a random float within range min max.
  Algorithm from /Collected Algorithms from the ACM/;
  133, 266, 294, 370."
  (+ (* (/ (random seed)
           seed)
        (- max 
           min))
     min))

(defun average-vectors (vectors)
  (let ((length (length vectors)))
    (map 'vector #'(lambda (n) (/ n length))
	 (apply #'map 'vector #'+ vectors))))

(defun euclidean-distance (a b)
  (sqrt (map-reduce #'square #'+ (mapcar #'- a b))))

(defun dot-prod (a b) (reduce #'+ (map 'simple-vector #'* a b)))

(defun square (n) (* n n))

(defun sum-of-squares (seq) (reduce #'+ (map 'simple-vector #'square seq)))

(defun cos-similarity (vec-1 vec-2)
  "Calculate the cosine similarity of two non-zero vectors."
  (/ (dot-prod vec-1 vec-2)
     (* (sqrt (sum-of-squares vec-1))
        (sqrt (sum-of-squares vec-2)))))

(defun safe-op (fn &rest args) 
  (let ((arglist (delete nil args)))
    (when arglist (reduce fn arglist))))

(defun strict-op (fn return &rest args)
  (unless (member nil args)
    (reduce fn args)))

(defun */ (arg-1 arg-2)
  (if (= 0 arg-2) 0 (/ arg-1 arg-2)))

(defun *max (&rest n) (if n (reduce #'max n) 0))

(defun closest-2-base (n) (expt 2 (floor (log n 2))))

(defun fy-shuffle (lst)
  "Non-destructive Fisher-Yates."
  (let ((length (length lst))
	(list (coerce lst 'vector))) 
    (dotimes (i (- length 2))
      (let* ((j (+ i (random length)))
	     (ith (aref list i))
	     (jth (aref list j)))
	(setf (aref list i) jth
	      (aref list j) ith))) 
    (coerce list 'list)))
