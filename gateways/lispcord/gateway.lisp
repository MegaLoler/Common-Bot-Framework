(in-package :lispcord-gateway.gateway)

;; lispcord

(defun make-lispcord-bot (token)
  "Setup a lispcord connection."
  ;; maybe try out the new event handler api instead
  (lc:watch #'lispcord-on-message lc:>message-create>)
  (lc:make-bot token))

(defmethod lispcord-on-message ((message lc:message))
  "Pass message events from lispcord to common gateway."
  (event-notify :message
		(make-gateway-message-from-lispcord-message
		 message)))

(defun make-gateway-message-from-lispcord-message (message)
  nil) ;todo



;; lispcord gateway

(defun lispcord-gateway-specifier-p (expression)
  "Whether an expression is a lispcord gateway specifier."
  (declare (type cons expression))
  (equalp (car expression) 'discord))

(defun lispcord-gateway (designator)
  "Return a lispcord gateway designated by a designator."
  (cond ((lispcord-gateway-p designator)
	 designator)
	((lispcord-gateway-specifier-p designator)
	 (make-lispcord-gateway :token (second designator)))
	(t (error "Invalid lispcord gateway designator!"))))

(def-gateway-specifier discord lispcord-gateway)

(defstruct (lispcord-gateway (:include gateway
				       (name "Discord"))
			     (:print-object lispcord-gateway-print))
  "Discord gateway via lispcord."
  (token nil :type string :read-only t)
  (bot nil :type (or null lc:bot)))

(defun lispcord-gateway-init (gateway)
  "Initialize a lispcord gateway object."
  (setf (lispcord-gateway-bot gateway)
	(lc:make-bot (lispcord-gateway-token gateway))))

(defun lispcord-gateway-designator (gateway)
  "Return an expression that designates a lispcord gateway object."
  `(discord ,(lispcord-gateway-token gateway)))

(defun lispcord-gateway-print (gateway stream)
  "Print a lispcord gateway object as a readable string."
  (format stream "~S" (lispcord-gateway-designator gateway)))



;; gateway interface

(defmethod gateway-connect ((gateway lispcord-gateway))
  "Connect to Discord."
  (lispcord-gateway-init gateway)
  (connect (lispcord-gateway-bot gateway)))

(defmethod gateway-disconnect ((gateway lispcord-gateway))
  "Disconnect from Discord."
  (disconnect (lispcord-gateway-bot gateway)))

(defmethod gateway-send ((gateway lispcord-gateway)
			 (string string)
			 (channel lispcord-channel))
  "Send a message on Discord."
  (lc:create string
	     (lc:from-id (channel-id channel)
			 :channel)
	     (lispcord-gateway-bot gateway)))
