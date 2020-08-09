(in-package #:jack.tools.https)

(defun jsonp (str)
  (and (stringp str)
       (not (string= str ""))
       (or (eq (char str 0) #\{)
	   (eq (char str 0) #\[))))

(defun htmlp (str)
  (and (stringp str)
       (substringp "<!DOCTYPE html>" str)))

(defun decode-http-body (body) 
  (cond ((and (stringp body) (jsonp body))
	 (jonathan:parse body :as :alist)) 
	((or (stringp body) (numberp body)) body)
	(t (decode-http-body
	    (babel:octets-to-string
	     (coerce body '(vector (unsigned-byte 8))))))))

(defun encode-http-body (body)
  (cond ((or (jsonp body) (htmlp body)) body) 
	((stringp body) (format nil "{\"message\": \"~d\"}" body))
	((listp body) (jonathan:to-json body :from :alist))
	(t (jonathan:to-json body))))

(defun local-address ()
  (let ((listener-thread (find-thread "handler")))
    (when listener-thread
      (cl-ppcre:scan-to-strings "\\d+\\.\\d+\\.\\d+\\.\\d+\\:\\d+"
				(bt:thread-name listener-thread)))))

(defmacro defhandler ((app uri &key class-map multicast (decode? t) (method :get)
				 (content-type "application/json")) &body body)
  `(setf (ningle:route ,app ,uri :method ,method)
	 #'(lambda (params)
	     (declare (ignorable params))
	     (setf (getf (response-headers ningle:*response*)
			 :content-type)
		   ,content-type)
	     (let ((http-content*
		     (cond (,multicast (cast-all (request-parameters ningle:*request*) ,class-map))
			   (,class-map (cast (request-parameters ningle:*request*) ,class-map))
			   (,decode? (request-parameters ningle:*request*))
			   (t (request-content ningle:*request*)))))
	       (declare (ignorable http-content*)) 
	       (encode-http-body (progn ,@body))))))
