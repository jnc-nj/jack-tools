(in-package :cl-user)

(defpackage #:jack.tools.threads
  (:use #:cl)
  (:export #:COUNT-THREADS
	   #:CONNECT-CLIENT
	   #:JOIN-THREAD
	   #:DESTROY-THREAD
	   #:FIND-THREAD
	   #:ALL-THREAD-NAMES))

(defpackage #:jack.tools.filesystems
  (:use #:cl
	#:cl-ppcre)
  (:export #:CREATE-DIRECTORY
           #:WRITE-FILE
           #:OPEN-FILE
           #:CMD-READ-PATH
           #:*PROBE-FILE
	   #:TRUENAMEP
	   #:GET-EXTENSION))

(defpackage #:jack.tools.lists
  (:use #:cl #:alexandria)
  (:export #:AGETHASH
	   #:AGETHASH-VALS
	   #:SPLIT-LIST
	   #:COMBINATIONS
	   #:ALISTP
	   #:TRIM-SEQ
	   #:TRIM-SORT
	   #:UNION-SORT
	   #:ALL-POSITIONS 
	   #:MAP-REDUCE
	   #:REDUCE-MAP
	   #:WINDOWS
           #:RANDOM-ITEM
           #:RANDOM-SELECTION
	   #:SET-EQUALS
	   #:DOTTED-PAIR-P
	   #:EVERY-LIST-P
	   #:ALIST-TO-PLIST
	   #:PUSH-ALL))

(defpackage #:jack.tools.misc
  (:use #:cl
	#:symbol-munger
	#:lack.response
	#:lack.request
	#:jack.tools.lists)
  (:export #:EMPTY-P
	   #:DEKEYWORDFY
	   #:KEYWORDFY 
	   #:PROMPT-READ
	   #:READ-FLAG
           #:IF-EXIST-RETURN
	   #:STRING-ALIST-VALUES
	   #:LIST-PACKAGE-SYMBOLS
	   #:LARGEST-KEY))

(defpackage #:jack.tools.objects
  (:use #:cl
	#:alexandria
	#:jonathan
	#:jack.tools.lists
	#:jack.tools.misc)
  (:export #:ADDRESS-BOOK
	   #:INTERFACES
	   #:STACK
	   #:COPY-INSTANCE
	   #:TO-ADDRESS-BOOK
	   #:CAST
	   #:CAST-ALL
	   #:CREATE-CLASS-MAP
	   #:FIND-CLASS-MAP
	   #:GET-CLASS-ID
	   #:GET-OBJECT-ID
	   #:GET-SLOT-NAMES
	   #:SLOTS-EQUAL?
	   #:OBJECTP
           #:OBJECT-TO-ALIST
	   #:GENERATE-JSON-METHOD
	   #:GENERATE-JSON-METHODS))

(defpackage #:jack.tools.https
  (:use #:cl
	#:lack.response
	#:lack.request
	#:jack.tools.objects
	#:jack.tools.lists)
  (:export #:JSONP
	   #:DECODE-HTTP-BODY
	   #:ENCODE-HTTP-BODY
	   #:DEFHANDLER
	   #:HTTP-CONTENT*))

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

(defpackage #:jack.tools.maths
  (:use #:cl
        #:jack.tools.lists)
  (:export #:RANDOM-FLOAT
	   #:AVERAGE-VECTORS
           #:EUCLIDEAN-DISTANCE
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

(defpackage #:jack.tools.keys
  (:use #:cl 
	#:jack.tools.objects
	#:jack.tools.lists
        #:jack.tools.maths
	#:jack.tools.misc
	#:jack.tools.filesystems
	#:jack.tools.https)
  (:export #:*PRNG*
	   
	   #:PANTS
	   #:BELTS
	   #:BRIEFS
	   #:PANTS-ON
	   #:PANTS-OFF
	   #:REMOVE-PANTS
	   #:REMOVE-BELTS
	   
	   #:READ-ENCODED-KEY
	   #:PARSE-PEM-FILE
	   #:TEST-KEYS
	   #:BYTE-ARRAY?
	   #:CREATE-HASH
	   #:CREATE-DIGEST
	   #:KEY-DISTANCE
	   #:PAD-KEY
	   #:CREATE-CUSTOM-KEY
	   #:TRIM-KEY
	   #:COMPRESS-BIGNUM
	   #:DECOMPRESS-BIGNUM
           #:USB8-ARRAY-TO-INTEGER
	   #:DECOMPRESS-KEY
	   #:RSA-ENCRYPT-MESSAGE
	   #:RSA-DECRYPT-MESSAGE
	   #:AES-ENCRYPT-MESSAGE
	   #:AES-DECRYPT-MESSAGE
	   #:SIGN-MESSAGE
	   #:VERIFY-SIGNATURE
	   #:CREATE-ID
           #:CREATE-RANDOM-PATH
	   #:MAKE-CIPHER
           #:GENERATE-PRIVATE-PEM
           #:GENERATE-PUBLIC-PEM
           #:GENERATE-PEMS
	   #:PUBKEYP))

(defpackage #:jack.tools.time
  (:use #:cl 
	#:jack.tools.maths)
  (:export #:SEC-NOW
           #:CREATE-TIME
           #:TIME-DIFFERENCE
           #:TIMESTAMP>
	   #:TIMESTAMP>=
           #:TIMED-INDEX
           #:TIMEOUT
           #:WAIT
           #:RELEASE))

(defpackage #:jack.tools.strings
  (:use #:cl)
  (:export #:RREPLACE
	   #:STRINGFY
	   #:SEXPP
	   #:SUBSTRINGP
	   #:REGEXFY
	   #:CONCATSTRING
	   #:GET-REGEX-MATCH
	   #:STRING-TEST-P
	   #:TRIM-WHITESPACE
	   #:BRACE-BALANCE-P))

(defpackage #:jack.tools.trees
  (:use #:cl
	#:jack.tools.lists
	#:jack.tools.maths)
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
	#:jack.tools.time
	#:jack.tools.threads
	#:jack.tools.https
	#:jack.tools.objects)
  (:export #:WITH-PROFILER
	   #:WITH-EXCEPTED-API
	   #:WITH-BT-THREAD
	   #:WITH-INFO
	   #:WITH-ENTER-LEAVE
	   #:WITH-TIMER
	   #:WITH-TIMED-LOOP
	   #:WITH-SUPPRESSED-OUTPUT
	   #:WITH-MUTLIPLE-SLOTS
	   #:WITH-SECURE-API
	   #:WITH-ENSURE-PACKAGE
	   #:SECURE-CONTENT*
	   #:WITH-QUERY
	   #:HTTP-BODY*
	   #:HTTP-CODE*
	   #:WITH-LOG))

(defpackage #:jack.tools.matrices
  (:use #:cl
	#:jack.tools.trees)
  (:export #:LOAD-MATRIX))

(defpackage #:jack.tools.bootstraps
  (:use #:cl
	#:cl-bootstrap
	#:cl-who
	#:jack.tools.withs
	#:jack.tools.lists
	#:jack.tools.keys)
  (:export #:CREATE-BOOTSTRAP-ID
	   #:BS-BTN
	   #:BS-BTN-LG
	   #:BS-BTN-SM
	   #:BS-BTN-XS
	   #:BS-BTN-DEFAULT
	   #:BS-BTN-PRIMARY
	   #:BS-BTN-SUCCESS
	   #:BS-BTN-INFO
	   #:BS-BTN-WARNING
	   #:BS-BTN-DANGER
	   #:BS-LINK-BTN
	   #:BS-BTN-DROPDOWN
	   #:BS-FORM-TEXT
	   #:BS-FORM-EMBD-STATIC
	   #:BS-FORM-EMBD-CHECKBOX
	   #:BS-EMBD-MODAL
	   #:BS-EMBD-TABLE))

(defpackage #:jack.tools.serialize
  (:use #:cl #:alexandria
	#:jack.tools.misc)
  (:export #:*SERIAL*
	   #:SERIAL-DICT
	   #:SERIAL-OBJECT
	   #:N2S-OF
	   #:S2N-OF
	   #:IX
	   #:SERIAL-UPDATE
	   #:SERIAL-READ
	   #:SERIAL-OUTPUT
	   #:SERIAL
	   #:SERIALIZE))
