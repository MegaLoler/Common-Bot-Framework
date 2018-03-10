(defpackage :common-bot
  (:documentation "Top level package.")
  (:use :cl)
  (:export bot
	   defbot
	   make-bot
	   bot-p
	   bot-name
	   bot-programmer
	   bot-documentation
	   bot-commands
	   bot-command-p
	   bot-command-by-alias

	   command
	   defcommand
	   make-command
	   command-p
	   command-aliases
	   command-documentation
	   command-examples
	   command-permitted
	   command-evaluator
	   command-print
	   command-suitable-alias
	   command-permitted-p
	   command-eval))

(defpackage :common-bot.commands
  (:documentation "Some useful general commands for bots.")
  (:use :cl :common-bot)
  (:export commands-command
	   info-command))
