(in-package :uncommon-bot.core)

(defclass bot-syntax (syntax)
  ()
  (:documentation "A printer/reader syntax for bot expressions."))

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object formatted-object)
     (stream stream)
     (environment bot-environment))
  "Print a formatted object."
  (uncommon-lisp-print (text-protocol environment) object stream environment))

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object localized-object)
     (stream stream)
     (environment bot-environment))
  "Print a localized object."
  (uncommon-lisp-print (language environment) object stream environment))

(defmethod uncommon-lisp-print
    ((syntax bot-syntax)
     (object verbal-expression)
     (stream stream)
     (environment bot-environment))
  "Print a verbal expression."
  (uncommon-lisp-print (personality environment) object stream environment))
