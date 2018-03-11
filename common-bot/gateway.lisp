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
  (when (bot-attend-p bot message)
    (bot-send bot message gateway
	      (bot-eval-message bot message)
	      (message-channel message))))

(defun bot-connect (bot gateway)
  "Connect a bot to a gateway."
  (let ((gateway (gateway gateway)))
    (gateway-add-listener
     gateway :connect
     (lambda (gateway)
       (bot-on-connect bot gateway)))
    (gateway-add-listener
     gateway :disconnect
     (lambda (gateway)
       (bot-on-disconnect bot gateway)))
    (gateway-add-listener
     gateway :message
     (lambda (gateway message)
       (bot-on-message bot gateway message)))
    (gateway-connect gateway)
    gateway))

(defun bot-disconnect (bot gateway)
  "Disconnect a bot from a gateway."
  (declare (ignorable bot))
  (gateway-disconnect gateway))

(defun bot-send (bot message gateway object channel)
  "Send an object to a channel on a gateway with a bot and originating message context."
  (gateway-send gateway
  		;(bot-localize bot message object)
  		(format nil "~A" (bot-localize bot message object));tmp until text formatting in gateway
  		channel))

(defun bot-localize (bot message object)
  "Localize an object according to a context."
  (declare (ignorable bot message)) ;fer now
  (localize-eval object nil nil)) ;; for now, todo 4 real

(defun bot-eval-message (bot message)
  "Read and evaluate a message from a gateway."
  ;; gonna put a simple error catching thing in place
  ;; but i want it to be better, and localizable, and formattable
  (handler-case
      (bot-eval bot message
		(bot-read
		 bot (message-content message)))
    (condition (condition)
      (format nil "Something went wrong: ~A" condition))))

(defun bot-attend-p (bot message)
  "Whether a bot should respond to a message or not."
  (declare (ignorable bot)) ;; this is just a start
  (and (not (user-botp (message-author message)))
       (message-prefixed-p bot
			   (message-content message))))
