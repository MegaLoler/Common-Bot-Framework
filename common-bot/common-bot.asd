(defsystem #:common-bot
    :description "A framework for building chatbots."
    :author "MegaLoler"
    :serial t
    :depends-on (#:common-gateway)
    :components ((:file "package")
		 (:file "bot")
		 (:file "command")
		 (:file "commands")
		 (:file "bot-expression")
		 (:file "bot-syntax")
		 (:file "bot-syntaxes")
		 (:file "util")))
