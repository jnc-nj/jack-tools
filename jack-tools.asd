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
	       (:file "src/https")
	       (:file "src/strings")
	       (:file "src/trees")
	       (:file "src/withs")
	       (:file "src/time")
	       (:file "src/cli")
	       (:file "src/bootstraps")
	       (:file "src/serialize"))
  :depends-on (:alexandria
	       :cl-ppcre
	       :cl-json
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
	       :dexador))
