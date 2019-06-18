(in-package #:jack.tools.https)

(defun jsonp (str)
  (eq (char str 0) #\{))

(defun decode-http-body (body &key (json? t))
  (cond ((and (stringp body) (jsonp body) json?)
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
			   (content-type "application/json"))
		      &body body)
  `(setf (ningle:route ,app ,uri :method ,method)
	 #'(lambda (params)
	     (declare (ignorable params))
	     (setf (getf (response-headers ningle:*response*)
			 :content-type)
		   ,content-type)
	     (let* ((request* (request-content ningle:*request*))
		    (http-content*
		     (cond (,multicast (cast-all (decode-http-body request*) ,class-map))
			   (,class-map (cast (decode-http-body request*) ,class-map))
			   (,decode? (decode-http-body request*))
			   (t request*))))
	       (encode-http-body (progn ,@body))))))
