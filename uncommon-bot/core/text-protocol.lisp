(in-package :uncommon-bot.core)

(defclass text-protocol ()
  ()
  (:documentation "A text formatting protocol."))

(defmethod uncommon-lisp-print
    ((text-protocol text-protocol)
     (object formatted-object)
     (stream stream)
     (environment bot-environment))
  "Perform a generic print of a formatted object."
  (prin1 object stream))
