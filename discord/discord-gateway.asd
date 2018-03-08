(defsystem #:discord-gateway
    :description "An implementation of the abstract chat service interface for Discord using Lispcord."
    :author "MegaLoler"
    :serial t
    :depends-on (#:gateway
		 #:lispcord)
    :components ((:file "discord")))
