(in-package :common-bot)

(defun bot-on-connect (bot gateway)
  (declare (ignorable bot gateway)) ;; for now
  "When a bot connects to a gateway."
  nil)

(defun bot-on-disconnect (bot gateway)
  "When a bot disconnects from a gateway."
  (declare (ignorable bot gateway)) ;; for now
  nil)

(defun bot-on-message (bot gateway message)
  "When a bot receives a message from a gateway."
  (declare (ignorable bot gateway message)) ;; for now
  nil)

(defun bot-connect (bot gateway)
  "Connect a bot to a gateway."
  (let ((gateway (gateway gateway)))
    (gateway-add-listener gateway :connect
			  (lambda (gateway)
			    (bot-on-connect bot gateway)))
    (gateway-add-listener gateway :disconnect
			  (lambda (gateway)
			    (bot-on-disconnect bot gateway)))
    (gateway-add-listener gateway :message
			  (lambda (gateway message)
			    (bot-on-message bot gateway message)))
    (gateway-connect gateway)
    gateway))

(defun bot-disconnect (bot gateway)
  "Disconnect a bot from a gateway."
  (declare (ignorable bot)) ;; for now
  (gateway-disconnect gateway))
