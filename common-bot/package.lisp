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
	   bot-prefixes
	   bot-language
	   bot-personality
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
	   bot-read
	   bot-read-string
	   message-prefixed-p

	   bot-connect
	   bot-disconnect
	   bot-send

	   localizable
	   localize
	   localize-eval
	   deflocalizable
	   deflocalization
	   language
	   make-language
	   language-p
	   language-code
	   language-name
	   personality
	   make-personality
	   personality-p
	   personality-name))

(defpackage :common-bot.common
  (:documentation "Some predefined general-purpose commands, strings, languages, and personalities.")
  (:use :cl :common-gateway :common-bot)
  (:export commands-command
	   info-command
	   dummy-command

	   commands-string
	   info-string

	   english-language
	   make-english-language
	   english-language-p
	   dutch-language
	   make-dutch-language
	   dutch-language-p

	   basic-personality
	   make-basic-personality
	   basic-personality-p))
