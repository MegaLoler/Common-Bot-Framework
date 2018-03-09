(defpackage :common-bot
  (:documentation "Top level package.")
  (:use :cl))

(defpackage :common-bot.commands
  (:documentation "Some useful general commands for bots.")
  (:use :cl :common-bot))

(defpackage :common-bot.syntaxes
  (:documentation "Some useful bot expression syntaxes for various chat services.")
  (:use :cl :common-bot))
