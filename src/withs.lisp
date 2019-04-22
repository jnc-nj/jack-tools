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

(defmacro with-timed-loop (title threshold delay print-condition &body body)
  `(when (and ,threshold (null (find-thread ,title)))
     (when ,delay (with-timer ,delay 1))
     (with-bt-thread ,title
       (handler-case (with-timer ,threshold nil ,@body)
         (sb-thread:join-thread-error () nil)))))

(defmacro with-suppressed-output (&body body)
  `(with-open-stream (*standard-output* (make-broadcast-stream))
     ,@body))

(defmacro with-secure-api (content aes-key private-key public-key &body body)
  `(pants-on ,aes-key ,private-key
	     (let ((secure-content*
                     (when ,content
                       (pants-off ,aes-key ,public-key ,content))))
	       ,@body)))

(defun verify-access-token (host uri access-token)
  (string= "success"
	   (agethash :message
		     (cl-json:decode-json-from-string
		      (decode-http-body
		       (with-excepted-api nil
			 (drakma:http-request (format nil "~d/~d" host uri)
					      :method :post :content-length t
					      :content-type "application/json"
					      :content (format nil "{\"access_token\": \"~d\"}"
							       access-token))))))))
