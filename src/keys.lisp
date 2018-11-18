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
	 (key-distance (base64:base64-string-to-usb8-array key-1) key-2))
	((stringp key-2)
	 (key-distance key-1 (base64:base64-string-to-usb8-array key-2)))
	(t (euclidean-distance (coerce key-1 'list) (coerce key-2 'list)))))

(defun pad-key (target-key reference-key)
  (cond ((stringp target-key)
         (pad-key (base64:base64-string-to-usb8-array target-key) reference-key))
        ((stringp reference-key)
         (pad-key target-key (base64:base64-string-to-usb8-array reference-key)))
        (t (let* ((reference-length (length reference-key))
                  (target-length (length target-key))
                  (length-difference (- reference-length target-length))
                  (final-key (append (coerce target-key 'list)
                                     (coerce (subseq reference-key target-length) 'list))))
             (base64:usb8-array-to-base64-string
              (make-array (length final-key) 
                          :element-type '(unsigned-byte 8)
                          :initial-contents final-key))))))

(defun trim-key (key &key (start 0) (end 16))
  (if (stringp key)
      (trim-seq key start end)
      (trim-key (decompress-key key) :start start :end end)))

(defun compress-bignum (bignum)
  (base64:integer-to-base64-string bignum))

(defun decompress-bignum (bignum)
  (base64:base64-string-to-integer bignum))

(defun usb8-array-to-integer (array)
  (base64:base64-string-to-integer
   (base64:usb8-array-to-base64-string array)))

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

(defun rsa-decrypt-message (private-key message &key string?)
  (cond ((stringp private-key)
	 (rsa-decrypt-message (decompress-key private-key) message
                              :string? string?))
	((byte-array? message)
         (let ((output (ironclad:decrypt-message private-key message)))
           (if string? (babel:octets-to-string output) output)))
	(t (rsa-decrypt-message private-key (base64:base64-string-to-usb8-array message)
                                :string? string?))))

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

(defun aes-decrypt-message (key iv message &key string?)
  (cond ((stringp key)
	 (aes-decrypt-message (base64:base64-string-to-usb8-array key) iv message
                              :string? string?))
	((stringp iv)
	 (aes-decrypt-message key (base64:base64-string-to-usb8-array iv) message
                              :string? string?))
	((byte-array? message)
	 (let ((_message (copy-seq message)))
	   (ironclad:decrypt-in-place (make-cipher key iv) _message)
	   (if string? (babel:octets-to-string _message) _message)))
	(t (aes-decrypt-message key iv (base64:base64-string-to-usb8-array message)
                                :string? string?))))

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

(defun create-id (&key (size 16) (string? t))
  (let ((id (ironclad:random-data size *prng*)))
    (if string?
        (base64:usb8-array-to-base64-string id)
        id)))

(defun create-random-path (path &key (size 16))
  (format nil "~d~d.txt" path (uuid:make-v4-uuid)))

(defun make-cipher (key-1 key-2)
  (ironclad:make-cipher
   :aes :key key-1
        :mode :cbc
        :initialization-vector key-2))

(defun pants-on (aes-key public-key object)
  "Takes a lisp or json object, then returns a json."
  (let ((iv (create-id :size 16 :string? nil))) 
    (format nil "{\"key\": \"~d\", \"body\": \"~d\"}"
	    (rsa-encrypt-message public-key iv)
	    (aes-encrypt-message aes-key iv object))))

(defun pants-off (aes-key private-key object &key (string? t))
  "Takes a json, then returns a lisp object."
  (let* ((json (cl-json:decode-json-from-string object))
	 (iv (rsa-decrypt-message private-key (agethash :key json)))
         (decryption (aes-decrypt-message aes-key iv (agethash :body json)
                                          :string? t))) 
    (values (if string? (cl-json:decode-json-from-string decryption) decryption)
            iv)))

(defun generate-private-pem (private-path &key (identifier (uuid:make-v4-uuid))) 
  (inferior-shell:run/nil
   `(progn (openssl genrsa -out ,(pathname (format nil "~d~d.private" private-path identifier)) 2048)))
  identifier)

(defun generate-public-pem (private-path identifier)
  (inferior-shell:run/nil
   `(progn (openssl rsa -in ,(pathname (format nil "~d~d.private" private-path identifier))
                    -outform PEM
                    -pubout -out ,(pathname (format nil "~d~d.public" private-path identifier)))))
  identifier)

(defun generate-pems (private-path &key (identifier (uuid:make-v4-uuid)))
  (generate-public-pem private-path (generate-private-pem private-path :identifier identifier)))
