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
  (declare (ignorable bot gateway message))
  ;; need to have a system of filterign out messages not directed at the bot!!!
  (bot-send bot message gateway
	    (bot-eval bot message (message-content message)) ;; also need to 'read' this first?
	    (message-channel message)))

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
		(bot-localize bot message object)
		channel))

(defun bot-localize (bot message object)
  "Localize an object according to a context."
  (declare (ignorable bot message)) ;fer now
  (localize-eval object nil nil)) ;; for now, todo 4 real
