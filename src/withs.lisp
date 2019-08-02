(in-package #:jack.tools.withs)

(defmacro with-profiler (packages times &body body)
  "Profile PACKAGES with BODY n TIMES."
  `(progn (sb-profile:profile ,@packages)
	  (dotimes (n ,times) ,@body)
	  (sb-profile:report)
	  (sb-profile:unprofile ,@packages)))

(defmacro with-excepted-api (exception &body body)
  `(handler-case ,@body
     (usocket:connection-refused-error () ,exception)))

(defmacro with-bt-thread (name &body body)
  `(labels ((fn nil ,@body))
     (bt:make-thread #'(lambda () (fn)) :name ,name)))

(defmacro with-info (info &body body)
  `(let ((start-time (sec-now)))
     (format t (format nil "~d..." ,info))
     ,@body
     (format t "...Done. [~ds]~%"
	     (- (sec-now) start-time))))

(defmacro with-enter-leave (name &body body)
  `(progn (log:info ,(format nil "[~d][entering sequence]" name))
	  ,@body
	  (log:info ,(format nil "[~d][leaving sequence]" name))))

(defmacro with-timer (interval iterations &body body)
  `(let* ((lock (bt:make-lock))
          (condition-variable (bt:make-condition-variable))
          (wheel (tw:make-wheel ,interval ,interval))
          (timer (tw:make-timer #'(lambda (wheel timer)
                                    (release lock condition-variable)
                                    (timeout wheel timer ,interval)))))
     (tw:with-timer-wheel wheel
       (timeout wheel timer ,interval)
       (if ,iterations
           (dotimes (iteration ,iterations)
             (wait lock condition-variable)
             ,@body)
           (loop do (wait lock condition-variable)
                ,@body)))))

(defmacro with-timed-loop (title threshold delay &body body)
  `(when (and ,threshold (null (find-thread ,title)))
     (when ,delay (with-timer ,delay 1))
     (with-bt-thread ,title
       (handler-case (with-timer ,threshold nil ,@body)
         (error () nil)))))

(defmacro with-suppressed-output (&body body)
  `(with-open-stream (*standard-output* (make-broadcast-stream))
     ,@body))

(defmacro with-multiple-slots (lst &rest body)
  `(let (collect)
     (dolist (item (reverse ,lst))
       (if collect
	   (setf collect `(with-slots ,(car item) ,(cadr item) ,collect))
	   (setf collect `(with-slots ,(car item) ,(cadr item) ,,@body))))
     collect))

(defmacro with-secure-api (content aes-key private-key public-key &body body)
  `(pants-on ,aes-key ,private-key
	     (let ((secure-content*
                    (when ,content
                      (pants-off ,aes-key ,public-key ,content))))
	       ,@body)))

(defmacro with-query (address (target payload &key class-map multicast) &body body)
  `(with-excepted-api nil
     (multiple-value-bind (http-body http-code*)
         (if (null ,payload)
	     (dex:get (format nil "http://~d/~d" ,address ,target))
	     (dex:post (format nil "http://~d/~d" ,address ,target)
		       :headers '(("Content-Type" . "application/json"))
		       :content (if (jsonp ,payload) ,payload
				    (jonathan:to-json ,payload))))
       (when (= 200 http-code*)
	 (let* ((_http-body (decode-http-body http-body))
		(http-body* (cond (,multicast (cast-all _http-body ,class-map))
				  (,class-map (cast _http-body ,class-map))
				  (t _http-body)))
		(body-output (progn ,@body)))
	   (values (if-exist-return body-output http-body*) http-code*))))))

(defmacro with-log (evaluator (&optional success-message &rest success-vars) (&optional fail-message &rest fail-vars) &body body)
  `(let ((evaluation (progn ,evaluator)))
     (if evaluation
	 (let ((output (progn ,@body)))
	   (when ,success-message (log:info ,success-message ,@success-vars))
	   (if-exist-return output t))
	 (log:warn ,fail-message ,@fail-vars))))
