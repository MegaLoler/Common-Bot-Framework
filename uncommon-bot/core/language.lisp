(in-package :uncommon-bot.core)

(defclass language ()
  ((name
    :initform :name
    :type localized-object
    :accessor name)
   (code
    :initform :code
    :type string
    :accessor code))
  (:documentation "A natural language."))

(defmethod uncommon-lisp-print
    ((language language)
     (object localized-object)
     (stream stream)
     (environment bot-environment))
  "Perform a generic print of a localized object."
  (prin1 object stream))
