(in-package #:jack.tools.time)

(defun universal-to-timestring (universal)
  (local-time:format-timestring nil (local-time:universal-to-timestamp universal)))

(defun sec-now ()
  (local-time:timestamp-to-universal (local-time:now)))

(defun create-time ()
  (local-time:format-timestring nil (local-time:now)))

(defun time-difference (time-1 time-2)
  (cond ((stringp time-1)
	 (time-difference (local-time:parse-timestring time-1) time-2))
	((stringp time-2)
	 (time-difference time-1 (local-time:parse-timestring time-2)))
	(t (- (local-time:timestamp-to-universal time-1)
	      (local-time:timestamp-to-universal time-2)))))

(defun year-difference (time-1 time-2)
  (cond ((stringp time-1)
	 (year-difference (local-time:parse-timestring time-1) time-2))
	((stringp time-2)
	 (year-difference time-1 (local-time:parse-timestring time-2)))
	(t (- (local-time:timestamp-year time-1)
	      (local-time:timestamp-year time-2)))))

(defun timestamp> (time-1 time-2)
  (cond ((null time-1) nil)
	((null time-2) t)
	((stringp time-1)
	 (timestamp> (local-time:parse-timestring time-1) time-2))
	((stringp time-2)
	 (timestamp> time-1 (local-time:parse-timestring time-2)))
	(t (local-time:timestamp> time-1 time-2))))

(defun timestamp>= (time-1 time-2)
  (cond ((null time-1) nil)
	((null time-2) t)
	((stringp time-1)
	 (timestamp>= (local-time:parse-timestring time-1) time-2))
	((stringp time-2)
	 (timestamp>= time-1 (local-time:parse-timestring time-2)))
	(t (local-time:timestamp>= time-1 time-2))))

(defun timed-index (timestamp interval objects &key return?)
  (let ((index (floor (mod (*/ (time-difference (create-time) timestamp)
			       interval)
			   (if (numberp objects)
			       objects
			       (length objects))))))
    (cond ((and (not (numberp objects)) return?) (nth index objects))
	  (t index))))

(defun timeout (wheel timer interval)
  (tw:schedule-timer wheel timer :milliseconds interval))

(defun wait (lock condition-variable)
  (bt:with-lock-held (lock)
    (bt:condition-wait condition-variable lock)))

(defun release (lock condition-variable)
  (bt:with-lock-held (lock)
    (bt:condition-notify condition-variable)))
