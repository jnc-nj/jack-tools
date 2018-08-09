(in-package #:jack.tools.keys)

(defvar *prng* (ironclad:make-prng :fortuna :seed :urandom))

(defun parse-pem-file (path)
  (cl-ppcre:regex-replace-all
   "(\\n|\\s*$)" (cdar (pem:parse-file (pathname path))) ""))

(defun test-keys (private-key public-key)
  (let ((temp (uuid:make-v4-uuid)))
    (ignore-errors
     (verify-signature
      public-key temp
      (sign-message private-key temp)))))

(defun byte-array? (input)
  (typep input '(simple-array (unsigned-byte 8))))

(defun create-hash (input)
  (base64:usb8-array-to-base64-string
   (create-digest input)))

(defun create-digest (input)
  (if (stringp input)
      (ironclad:digest-sequence
       :sha256 (ironclad:ascii-string-to-byte-array input)) 
      (create-digest (cl-json:encode-json-to-string input))))

(defun key-distance (key-1 key-2)
  (cond ((stringp key-1)
	 (key-distance (decompress-key key-1 :private? nil) key-2))
	((stringp key-2)
	 (key-distance key-1 (decompress-key key-2 :private? nil)))
	(t (with-slots ((n-1 ironclad::n) (e-1 ironclad::e)) key-1
	     (with-slots ((n-2 ironclad::n) (e-2 ironclad::e)) key-2
	       (let ((delta-n (- (coerce n-1 'double-float) (coerce n-2 'double-float)))
		     (delta-e (- (coerce e-1 'double-float) (coerce e-2 'double-float))))
		 (sqrt (+ (* delta-n delta-n) (* delta-e delta-e)))))))))

(defun pad-key (target-key reference-key)
  (let* ((reference-length (length reference-key))
         (target-length (length target-key))
         (length-difference (- reference-length target-length)))
    (concatenate 'string target-key (subseq reference-key 0 length-difference))))

(defun trim-key (key &key (start 0) (end 16))
  (if (stringp key)
      (trim-seq key start end)
      (trim-key (decompress-key key) :start start :end end)))

(defun compress-bignum (bignum)
  (base64:integer-to-base64-string bignum))

(defun decompress-bignum (bignum)
  (base64:base64-string-to-integer bignum))

(defun decompress-key (key &key (private? t))
  (if private?
      (pem/pkey::read-private-key key)
      (pem/pkey::read-public-key key)))

(defun rsa-encrypt-message (public-key message)
  (cond ((stringp public-key)
	 (rsa-encrypt-message (decompress-key public-key :private? nil) message))
	((byte-array? message)
	 (base64:usb8-array-to-base64-string (ironclad:encrypt-message public-key message)))
	(t (rsa-encrypt-message public-key (ironclad:ascii-string-to-byte-array message)))))

(defun rsa-decrypt-message (private-key message)
  (cond ((stringp private-key)
	 (rsa-decrypt-message (decompress-key private-key) message))
	((byte-array? message)
	 (babel:octets-to-string (ironclad:decrypt-message private-key message)))
	(t (rsa-decrypt-message private-key (base64:base64-string-to-usb8-array message)))))

(defun aes-encrypt-message (key iv message)
  (cond ((stringp key)
	 (aes-encrypt-message (base64:base64-string-to-usb8-array key) iv message))
	((stringp iv)
	 (aes-encrypt-message key (base64:base64-string-to-usb8-array iv) message))
	((byte-array? message)
	 (let ((_message (copy-seq message)))
	   (ironclad:encrypt-in-place (make-cipher key iv) _message)
	   (base64:usb8-array-to-base64-string _message)))
	(t (aes-encrypt-message key iv (ironclad:ascii-string-to-byte-array message)))))

(defun aes-decrypt-message (key iv message)
  (cond ((stringp key)
	 (aes-decrypt-message (base64:base64-string-to-usb8-array key) iv message))
	((stringp iv)
	 (aes-decrypt-message key (base64:base64-string-to-usb8-array iv) message))
	((byte-array? message)
	 (let ((_message message))
	   (ironclad:decrypt-in-place (make-cipher key iv) _message)
	   (babel:octets-to-string _message)))
	(t (aes-decrypt-message key iv (base64:base64-string-to-usb8-array message)))))

(defun sign-message (private-key message)
  (cond ((stringp private-key)
	 (sign-message (decompress-key private-key) message))
	((byte-array? message)
	 (base64:usb8-array-to-base64-string (ironclad:sign-message private-key message)))
	(t (sign-message private-key (create-digest message)))))

(defun verify-signature (public-key message signature)
  (cond ((stringp signature)
	 (verify-signature public-key message (base64:base64-string-to-usb8-array signature)))
	((stringp public-key)
	 (verify-signature (decompress-key public-key :private? nil) message signature))
	((byte-array? message)
	 (ironclad:verify-signature public-key message signature))
	(t (verify-signature public-key (create-digest message) signature))))

(defun create-id (&key (size 16))
  (base64:usb8-array-to-base64-string
   (ironclad:random-data size *prng*)))

(defun make-cipher (key-1 key-2)
  (ironclad:make-cipher :aes :key key-1
			:mode :cbc
			:initialization-vector key-2))

(defun pants-on (aes-key public-key object)
  "Takes a lisp or json object, then returns a json."
  (let ((iv (create-id :integer? nil)))
    (format nil "{\"key\": \"~d\", \"body\": \"~d\"}"
	    (rsa-encrypt-message public-key iv)
	    (aes-encrypt-message aes-key iv object))))

(defun pants-off (aes-key private-key object)
  "Takes a json, then returns a lisp object."
  (let* ((json (cl-json:decode-json-from-string object))
	 (iv (rsa-decrypt-message private-key (agethash :key json))))
    (values (cl-json:decode-json-from-string
	     (aes-decrypt-message aes-key iv (agethash :body json)))
	    iv)))

(defun generate-private-pem (private-path &key (identifier (uuid:make-v4-uuid))) 
  (inferior-shell:run/nil
   `(progn (openssl genrsa -out ,(pathname (format nil "~d~d.private" private-path identifier)) 2048)))
  identifier)

(defun generate-public-pem (private-path identifier)
  (inferior-shell:run/nil
   `(progn (openssl rsa -in ,(pathname (format nil "~d~d.private" private-path identifier))
                    -outform PEM
                    -pubout -out ,(pathname (format nil "~d~d.public" private-path identifier))))))

(defun generate-pems (private-path &key (identifier (uuid:make-v4-uuid)))
  (generate-public-pem private-path (generate-private-pem private-path :identifier identifier)))
