(in-package :cl-user)

(defpackage #:jack.tools.lists
  (:use #:cl)
  (:export #:AGETHASH
	   #:AGETHASH-VALS
	   #:SPLIT-LIST
	   #:COMBINATIONS
	   #:ALISTP
	   #:TRIM-SEQ
	   #:TRIM-SORT
	   #:ALL-POSITIONS 
	   #:MAP-REDUCE
	   #:WINDOWS))

(defpackage #:jack.tools.misc
  (:use #:cl
	#:jack.tools.lists)
  (:export #:KEYWORDFY 
	   #:PROMPT-READ
	   #:READ-FLAG
	   #:COUNT-THREADS
	   #:DECODE-HTTP-BODY))

(defpackage #:jack.tools.objects
  (:use #:cl
	#:alexandria
	#:jack.tools.lists)
  (:export #:ADDRESS-BOOK
	   #:INTERFACES
	   #:STACK
	   #:COPY-INSTANCE
	   #:TO-ADDRESS-BOOK
	   #:CAST 
	   #:CREATE-CLASS-MAP
	   #:FIND-CLASS-MAP
	   #:GET-CLASS-ID
	   #:GET-OBJECT-ID
	   #:GET-SLOT-NAMES
	   #:SLOTS-EQUAL?
	   #:OBJECTP))

(defpackage #:jack.tools.cli
  (:use #:cl
	#:alexandria
	#:jack.tools.lists
	#:jack.tools.misc
	#:jack.tools.objects)
  (:export #:MENU
	   #:NAME
	   #:PROMPT
	   #:FORM
	   #:LINKS
	   #:FUNCTIONS
	   #:RUN
	   #:EXIT
	   #:LOAD-MENU
	   #:NAME-OF
	   #:NEXT-LINK
	   #:GET-SUB-MENU
	   #:ADD-SUB-MENU
	   #:ADD-RUN
	   #:ADD-FUNCTION
	   #:WITH-CLI))

(defpackage #:jack.tools.keys
  (:use #:cl
	#:jack.tools.objects
	#:jack.tools.lists)
  (:export #:+EXPONENT+
	   #:+BASE+
	   #:+BIGN+
	   #:BYTE-ARRAY?
	   #:CREATE-HASH
	   #:CREATE-DIGEST
	   #:KEY-DISTANCE
	   #:PAD-KEY
	   #:COMPRESS-BIGNUM
	   #:DECOMPRESS-BIGNUM
	   #:COMPRESS-KEY
	   #:DECOMPRESS-KEY
	   #:RSA-ENCRYPT-MESSAGE
	   #:RSA-DECRYPT-MESSAGE
	   #:AES-ENCRYPT-MESSAGE
	   #:AES-DECRYPT-MESSAGE
	   #:SIGN-MESSAGE
	   #:VERIFY-SIGNATURE
	   #:CREATE-ID
	   #:MAKE-CIPHER
	   #:PANTS-ON
	   #:PANTS-OFF))

(defpackage #:jack.tools.maths
  (:use #:cl)
  (:export #:RANDOM-FLOAT
	   #:AVERAGE-VECTORS
	   #:DOT-PROD
	   #:SQUARE
	   #:SUM-OF-SQUARES
	   #:COS-SIMILARITY
	   #:SAFE-OP
	   #:STRICT-OP
	   #:*/
	   #:*MAX
	   #:CLOSEST-2-BASE
	   #:FY-SHUFFLE))

(defpackage #:jack.tools.time
  (:use #:cl
	#:jack.tools.maths)
  (:export #:SEC-NOW
	   #:CREATE-TIME
	   #:TIME-DIFFERENCE
	   #:TIMESTAMP>
	   #:TIMED-INDEX))

(defpackage #:jack.tools.strings
  (:use #:cl)
  (:export #:RREPLACE
	   #:STRINGFY
	   #:SEXPP
	   #:SUBSTRINGP
	   #:REGEXFY
	   #:CONCATSTRING
	   #:GET-REGEX-MATCH))

(defpackage #:jack.tools.trees
  (:use #:cl
	#:jack.tools.lists)
  (:export #:TREE-SIMILARITY
	   #:UPSILON
	   #:COMPARE-TREES
	   #:ELIMINATE
	   #:SUBBER
	   #:UNQUOTE
	   #:RECURSIVE-ALIST-HASH-TABLE))

(defpackage #:jack.tools.withs
  (:use #:cl
	#:jack.tools.lists
	#:jack.tools.misc
	#:jack.tools.keys
	#:jack.tools.time)
  (:export #:WITH-EXCEPTED-API
	   #:WITH-BT-THREAD
	   #:WITH-INFO
	   #:WITH-TIMED-LOOP
	   #:WITH-SECURE-API
	   #:SECURE-CONTENT*))

(defpackage #:jack.tools.matrices
  (:use #:cl
	#:jack.tools.trees)
  (:export #:LOAD-MATRIX))

(defpackage #:jack.tools
  (:use #:cl #:alexandria
	#:jack.tools.keys
	#:jack.tools.maths
	#:jack.tools.objects
	#:jack.tools.strings
	#:jack.tools.lists
	#:jack.tools.trees
	#:jack.tools.withs
	#:jack.tools.misc
	#:jack.tools.matrices))
