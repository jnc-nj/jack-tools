(in-package #:jack.tools.https)

(defun jsonp (str)
  (and (stringp str)
       (not (string= str ""))
       (or (eq (char str 0) #\{)
	   (eq (char str 0) #\[))))

(defun decode-http-body (body)
  (cond ((and (stringp body) (jsonp body))
	 (cl-json:decode-json-from-string body))
	((or (stringp body) (numberp body)) body)
	(t (decode-http-body
	    (babel:octets-to-string
	     (coerce body '(vector (unsigned-byte 8))))))))

(defun encode-http-body (body)
  (cond ((stringp body) body)
	((alistp body) (jonathan:to-json body :from :alist))
	(t (jonathan:to-json body))))

(defmacro defhandler ((app uri &key class-map multicast (decode? t) (method :get)
			   (content-type "application/json")) &body body)
  `(setf (ningle:route ,app ,uri :method ,method)
	 #'(lambda (params)
	     (declare (ignorable params))
	     (setf (getf (response-headers ningle:*response*)
			 :content-type)
		   ,content-type)
	     (log:info (request-headers ningle:*request*))
	     (let* ((request* (request-content ningle:*request*))
		    (http-content*
		     (cond (,multicast (cast-all (decode-http-body request*) ,class-map))
			   (,class-map (cast (decode-http-body request*) ,class-map))
			   (,decode? (decode-http-body request*))
			   (t request*))))
	       (declare (ignorable http-content*))
	       (log:info (babel:octets-to-string (coerce request* '(vector (unsigned-byte 8)))))
	       (encode-http-body (progn ,@body))))))
