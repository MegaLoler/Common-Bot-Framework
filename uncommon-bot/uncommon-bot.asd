(defsystem #:uncommon-bot
    :description "A simple language for my bots."
    :author "MegaLoler"
    :serial t
    :depends-on (#:uncommon-lisp)
    :components ((:module "core"
			  :components
                          ((:file "package")
			   (:file "bot-environment")
                           (:file "command")
                           (:file "formatted-object")
                           (:file "localized-object")
			   (:file "verbal-expression")
                           (:file "text-protocol")
			   (:file "language")
                           (:file "personality")
                           (:file "syntax")
			   (:file "semantics")
                           (:file "repl")))
  (:module "base"
   :components
   ((:file "package")
    (:file "basic-environment")
    (:file "commands")))))
