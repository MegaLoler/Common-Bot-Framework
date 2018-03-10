(defsystem #:common-bot
    :description "A framework for building chatbots."
    :author "MegaLoler"
    :serial t
    :depends-on (#:common-gateway)
    :components ((:file "package")
		 (:file "command")
		 (:file "bot")
		 (:file "commands")
		 (:file "bot-expression")
  (:file "gateway")))
