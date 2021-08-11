(in-package #:cl-user)
(asdf:defsystem jack-tools
  :author "Jack Nai-Chieh Chou <jacknchou@icloud.com>"
  :maintainer "Jack Nai-Chieh Chou <jacknchou@icloud.com>"
  :serial t
  :components ((:file "src/packages")
	       (:file "src/threads")
	       (:file "src/filesystems")
	       (:file "src/keys")
	       (:file "src/lists")
	       (:file "src/maths")
	       (:file "src/misc")
	       (:file "src/objects")
	       (:file "src/couchdb")
	       (:file "src/https")
	       (:file "src/strings")
	       (:file "src/trees")
	       (:file "src/withs")
	       (:file "src/time")
	       (:file "src/cli")
	       (:file "src/bootstraps")
	       (:file "src/serialize")
	       (:file "src/mysql"))
  :depends-on (:alexandria
	       :cl-ppcre
	       :cl-json
	       :cl-fad
	       :ironclad
	       :split-sequence
	       :local-time
	       :babel
	       :uuid
	       :bordeaux-threads
	       :cl-base64
	       :log4cl
               :pem
               :asn1
               :timer-wheel
               :inferior-shell
               :uuid
	       :cl-bootstrap
	       :jonathan
	       :symbol-munger
	       :closer-mop
	       :usocket
	       :ningle
	       :lack
	       :drakma
	       :dexador
	       :clouchdb
	       :cl-mysql
	       :cl-dbi
	       :sxql))
