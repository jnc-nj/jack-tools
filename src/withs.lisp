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

(defmacro with-timed-loop (title threshold fn print-condition &key delay)
  `(when ,threshold
     (when ,delay (sleep ,delay))
     (with-bt-thread ,(format nil "[THREAD][~d]" title)
       (loop do (sleep ,threshold) ,fn
	    (when ,print-condition (log:info ,(format nil "[UPDATE][~d]" title)))))))

(defmacro with-secure-api (content aes-key private-key public-key &body body)
  `(pants-on ,aes-key ,private-key
	     (let ((secure-content*
		    (when ,content
		      (pants-off ,aes-key ,public-key ,content))))
	       ,@body)))
