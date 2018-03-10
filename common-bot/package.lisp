(defpackage :common-bot
  (:documentation "Top level package.")
  (:use :cl :common-gateway)
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
	   command-eval

	   bot-expression
	   bot-eval

	   bot-connect
	   bot-disconnect
	   bot-send

	   localizable
	   localize
	   localize-eval
	   deflocalizable
	   deflocalization
	   language
	   personality))

(defpackage :common-bot.common
  (:documentation "Some predefined general-purpose commands, strings, languages, and personalities.")
  (:use :cl :common-bot)
  (:export commands-command
	   info-command))
