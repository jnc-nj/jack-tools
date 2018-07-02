(in-package #:jack.tools.withs)

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
  `(when ,threshold
     (when ,delay (with-timer ,delay 1))
     (with-bt-thread ,(format nil "[THREAD][~d]" title)
       (with-timer ,threshold nil
         ,@body
         (when ,print-condition
           (log:info ,(format nil "[UPDATE][~d]" title)))))))

(defmacro with-secure-api (content aes-key private-key public-key &body body)
  `(pants-on ,aes-key ,private-key
	     (let ((secure-content*
                     (when ,content
                       (pants-off ,aes-key ,public-key ,content))))
	       ,@body)))
