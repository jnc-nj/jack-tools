(in-package #:jack.tools.withs)

(defvar *with-enter-leave-print* nil)

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
     (destroy-thread ,name)
     (bt:make-thread #'(lambda () (fn)) :name ,name)))

(defmacro with-info (info &body body)
  `(let ((start-time (sec-now)))
     (format t (format nil "~d..." ,info))
     ,@body
     (format t "...Done. [~ds]~%"
	     (- (sec-now) start-time))))

(defmacro with-enter-leave (name &body body)
  `(progn (when *with-enter-leave-print*
	    (log:info ,(format nil "[~d][entering sequence]" name)))
	  ,@body
	  (when *with-enter-leave-print*
	    (log:info ,(format nil "[~d][leaving sequence]" name)))))

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

(defmacro with-query (address (target payload
			       &key params class-map multicast headers 
				 (version 1.1) (client :dexador) (protocol "http")
				 (encoder :jonathan) (decoder :jonathan)
				 form-data dex-cookie-jar drakma-cookie-jar)
		      &body body)
  `(with-excepted-api nil
     (multiple-value-bind (http-body http-code*)
         (let ((content (cond ((null ,payload) nil)
			      ((or (jsonp ,payload) (and ,form-data (alistp ,payload))) ,payload)
			      ((and (eq ,encoder :jonathan) (alistp ,payload))
			       (jonathan:to-json ,payload :from :alist))
			      ((alistp ,payload) (cl-json:encode-json-to-string ,payload))
			      ((eq ,encoder :jonathan) (jonathan:to-json ,payload))
			      (t (cl-json:encode-json-to-string ,payload))))
	       (url (cond ((eq ,client :dexador)
			   (format nil "~d://~d/~d~:[~;?~{~{~d=~d~}~^&~}~]"
				   ,protocol ,address ,target ,params ,params))
			  ((eq ,client :drakma)
			   (format nil "~d://~d/~:[~;~d~]"
				   ,protocol ,address ,target ,target)))))
	   (cond ((and (null content) (eq ,client :dexador))
		  (dex:get url
			   :version ,version
			   :cookie-jar ,dex-cookie-jar))
		 ((and (null content) (eq ,client :drakma))
		  (drakma:http-request url
				       :method :get
				       :cookie-jar ,drakma-cookie-jar)) 
		 ((eq ,client :dexador)
		  (dex:post url
			    :version ,version
			    :headers (unless ,form-data
				       (append ,headers
					       '(("Content-Type" . "application/json"))))
			    :content content
			    :cookie-jar ,dex-cookie-jar))
		 ((eq ,client :drakma)
		  (drakma:http-request url
				       :method :post
				       :content-type "application/json"
				       :content content
				       :cookie-jar ,drakma-cookie-jar))))
       (when (= 200 http-code*)
	 (let* ((_http-body (decode-http-body http-body :decoder ,decoder))
		(http-body* (cond (,multicast (cast-all _http-body ,class-map))
				  (,class-map (cast _http-body ,class-map))
				  (t _http-body)))
		(body-output (progn ,@body)))
	   (values (if-exist-return body-output http-body*) http-code*))))))

(defun connection-alive-p (address)
  (when address
    (handler-case (with-query address ("" nil))
      (error () nil))))

(defmacro with-log (evaluator (&optional success-message &rest success-vars) (&optional fail-message &rest fail-vars) &body body)
  `(let ((evaluation (progn ,evaluator)))
     (if evaluation
	 (let ((output (progn ,@body)))
	   (when ,success-message (log:info ,success-message ,@success-vars))
	   (if-exist-return output t))
	 (log:warn ,fail-message ,@fail-vars))))

(defmacro with-ensure-package ((gc name &rest dependencies) &body body)
  `(let ((pname (keywordfy ,name))
	 (current-package (keywordfy (package-name *package*))))
     (eval `(defpackage ,pname (:use ,,@(mapcar #'keywordfy dependencies))))
     (eval `(in-package ,pname))
     (let* ((output (multiple-value-list (progn ,@body)))
	    (str (write-to-string (gensym)))
	    (sym (intern str pname))) 
       (setf (symbol-value sym) output)
       (export sym pname)
       (eval `(in-package ,current-package)) 
       (let ((out (symbol-value (find-symbol str (find-package pname))))) 
	 (when ,gc
	   (handler-case (delete-package pname)
	     (error () nil))) 
	 out))))

(defmacro with-db-query ((connection
			  &key top-result-only first-element-only
			    no-duplicates (no-index t))
			 &body statements)
  `(eval (_with-db-query ,connection
			 ,@statements
			 ,top-result-only
			 ,first-element-only
			 ,no-duplicates
			 ,no-index)))

(defun _with-db-query (connection statements
		       top-result-only first-element-only
		       no-duplicates no-index)
  `(dbi:with-connection (conn ,@connection)
     (multiple-value-bind (string params) (yield ,statements)
       (let* ((prep (dbi:prepare conn string))
	      (exec (dbi:execute prep params))
	      (fetch (loop for row = (dbi:fetch exec)
			   while row
			   collect (alexandria:plist-alist row))))
	 (when ,no-index
	   (setf fetch (loop for item in fetch collect (mapcar #'cdr item))))
	 (when ,first-element-only
	   (setf fetch (mapcar #'car fetch)))
	 (when ,top-result-only
	   (setf fetch (car fetch)))
	 (when ,no-duplicates
	   (setf fetch (remove-duplicates fetch :test #'equal)))
	 fetch))))
