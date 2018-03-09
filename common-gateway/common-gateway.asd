(defsystem #:common-gateway
    :description "Abstract interface to chat services."
    :author "MegaLoler"
    :serial t
    :components ((:file "package")
		 (:file "gateway")
		 (:file "event")
		 (:file "server")
		 (:file "channel")
		 (:file "message")
		 (:file "user")
		 (:file "util")))
