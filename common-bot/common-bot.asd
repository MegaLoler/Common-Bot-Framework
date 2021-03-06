(defsystem #:common-bot
    :description "A framework for building chatbots."
    :author "MegaLoler"
    :serial t
    :depends-on (#:common-gateway)
    :components ((:file "package")
		 (:file "util")
		 (:file "command")
		 (:file "bot")
		 (:file "bot-expression")
		 (:file "gateway")
		 (:file "localizable")
		 (:file "language")
		 (:file "personality")
		 (:module "common"
			  :components
			  ((:file "languages")
			   (:file "personalities")
			   (:file "strings")
			   (:file "commands")))))
