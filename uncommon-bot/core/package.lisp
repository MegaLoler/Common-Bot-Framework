(defpackage :uncommon-bot.core
  (:documentation "A bot interaction language.")
  (:use :cl :uncommon-lisp)
  (:export bot-environment
	   parent
	   syntax
	   semantics
	   text-protocol
	   language
	   personality
	   commands
	   binding
	   bind
	   push-command
	   remove-command

	   command
	   aliases
	   description
	   eval-args
	   permitted-p
	   evaluate
	   invoke

	   formatted-object
	   localized-object
	   verbal-expression

	   text-protocol
	   language
	   personality

	   bot-syntax
	   bot-semantics
	   truep
	   falsep
	   truth-wrap
	   true
	   false

	   uncommon-bot-repl
	   uncommon-bot-load
	   uncommon-bot-load-string
	   uncommon-bot-load-file

	   uncommon-lisp-read
	   uncommon-lisp-print
	   uncommon-lisp-eval))
