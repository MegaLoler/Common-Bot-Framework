(defsystem #:lispcord-gateway
    :description "An implementation of the abstract chat service interface Common Gateway for Discord using Lispcord."
    :author "MegaLoler"
    :serial t
    :depends-on (#:common-gateway
		 #:lispcord)
    :components ((:file "package")
		 (:file "structs")
		 (:file "gateway")
		 (:file "server")
		 (:file "channel")
		 (:file "message")
		 (:file "user")
		 (:file "formatted")
		 (:file "emoji")))
