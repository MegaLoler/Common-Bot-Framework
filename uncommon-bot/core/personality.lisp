(in-package :uncommon-bot.core)

(defclass personality ()
  ((name
    :initarg :name
    :type verbal-expression
    :accessor name))
  (:documentation "A personality for verbal expressions."))

(defmethod uncommon-lisp-print
    ((personality personality)
     (object verbal-expression)
     (stream stream)
     (environment bot-environment))
  "Perform a generic print of a verbal expression."
  (prin1 object stream))
