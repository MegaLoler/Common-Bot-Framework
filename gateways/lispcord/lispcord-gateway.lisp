(defpackage :lispcord-gateway
  (:use :cl :gateway))
(in-package :lispcord-gateway)

;; lispcord

(defun make-lispcord-bot (token)
  (lc:watch #'lispcord-on-message lc:>message-create>)
  (lc:make-bot token))

(defmethod lispcord-on-message ((message lc:message))
  (event-notify :message
		(make-gateway-message-from-lispcord-message
		 message)))

(defun make-gateway-message-from-lispcord-message (message)
  )

;; gateway interface

(defmethod gateway-connect ((gateway lispcord-gateway))
  "Connect to Discord."
  (connect (lispcord-gateway-bot gateway)))

(defmethod gateway-disconnect ((gateway lispcord-gateway))
  "Disconnect from Discord."
  (disconnect (lispcord-gateway-bot gateway)))

(defmethod gateway-send ((gateway lispcord-gateway)
			 (message message))
  "Send a message on Discord."
  (lc:create (make-message (message-content message))
	     (message-channel message)
	     (lispcord-gateway-bot gateway)))
