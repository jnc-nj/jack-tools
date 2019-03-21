(in-package #:cl-user)
(asdf:defsystem jack-tools
  :author "Jack Nai-Chieh Chou <jacknchou@icloud.com>"
  :maintainer "Jack Nai-Chieh Chou <jacknchou@icloud.com>"
  :serial t
  :components ((:file "src/packages")
	       (:file "src/keys")
	       (:file "src/lists")
	       (:file "src/maths")
	       (:file "src/misc")
	       (:file "src/objects")
	       (:file "src/strings")
	       (:file "src/trees")
	       (:file "src/withs")
	       (:file "src/time")
	       (:file "src/cli")
	       (:file "src/bootstraps")
	       (:file "src/serialize")
	       ;;(:file "src/matrices")
	       )
  :depends-on (:alexandria
	       :cl-ppcre
	       :cl-json
	       :ironclad
	       :split-sequence
	       :local-time
	       :babel
	       :uuid
	       :drakma
	       :bordeaux-threads
	       :cl-base64
	       :log4cl
               :pem
               :asn1
               :timer-wheel
               :inferior-shell
               :uuid
	       :cl-bootstrap
	       ;;:mgl-mat
	       ))
