(defsystem #:common-bot
  :description "A framework for building chatbots."
  :author "MegaLoler"
  :serial t
  :components ((:file "package")
			 (:file "util")
			 (:file "bot")
			 (:file "command")
			 (:file "bot-expression")
			 (:file "commands")))
