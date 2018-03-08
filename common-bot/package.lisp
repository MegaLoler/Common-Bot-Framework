(defpackage :common-bot
  (:documentation "Top level package.")
  (:use :cl
	:common-bot.bot
	:common-bot.command
	:common-bot.bot-expression
	:common-bot.bot-syntax
	:common-bot.util))

(defpackage :common-bot.bot
  (:documentation "Bot definitions.")
  (:use :cl))

(defpackage :common-bot.command
  (:documentation "Command definitions.")
  (:use :cl))

(defpackage :common-bot.commands
  (:documentation "Predefined commands.")
  (:use :cl))

(defpackage :common-bot.bot-expression
  (:documentation "Bot expression semantics.")
  (:use :cl))

(defpackage :common-bot.bot-syntax
  (:documentation "Define bot syntax.")
  (:use :cl))

(defpackage :common-bot.bot-syntaxes
  (:documentation "Predefined bot syntaxes.")
  (:use :cl))

(defpackage :common-bot.util
  (:documentation "Misc. functions.")
  (:use :cl))
