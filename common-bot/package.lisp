(defpackage :bot.util
  (:documentation "Utility functions used in Common Bot Framework.")
  (:use :cl)
  (:export lambda-list-keyword
	   starts-with-p
	   starts-with-any))

(defpackage :bot.commands
  (:documentation "A collection of useful bot command definitions.")
  (:use :cl :bot)
  (:export commands command command-list show-commands
	   hug give-hug))

(defpackage :bot
  (:documentation "Build bots and commands and deal with bot expressions.")
  (:use :cl :bot.util)
  (:export defbot
	   make-bot
	   bot-name
	   bot-author
	   bot-documentation
	   bot-prefix
	   bot-expression-reader
	   bot-expression-printer
	   bot
	   bot-commands
	   bot-known-command-p
	   bot-reasonable-prefix
	   bot-reasonable-prefix-string
	   defcommand
	   make-command
	   command-aliases
	   command-documentation
	   command-permitted-p
	   command-examples
	   command-lambda-list
	   command-function
	   command
	   apply-command
	   commnad-call
	   command-allowed
	   print-alias
	   command-reasonable-alias
	   command-print-reasonable-alias
	   command-aliases-formatted
	   format-parameter
	   lambda-list-template-expression
	   command-template-expression
	   command-print-syntax
	   command-description
	   bot-expression
	   bot-eval
	   parse-bot-expression
	   read-bot-expression
	   print-bot-expression
	   read-bot-command
	   default-bot-expresssion-reader
	   default-bot-expression-printer))
