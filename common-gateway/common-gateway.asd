(defsystem #:common-gateway
  :description "Abstract interface to chat services."
  :author "MegaLoler"
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "event")
	       (:file "gateway")
	       (:file "server")
	       (:file "channel")
	       (:file "message")
	       (:file "user")
	       (:file "formatted")
	       (:file "emoji")))
