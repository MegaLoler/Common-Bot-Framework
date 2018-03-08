(defpackage :discord-gateway
  (:use :cl :lispcord :gateway))
(in-package :discord-gateway)

;; lispcord

(defun make-lispcord-bot (token)
  

;; gateway interface

(defmethod gateway-connect ((gateway discord-gateway))
  "Connect to Discord."
  (connect (gateway-lispcord-bot gateway)))

(defmethod gateway-disconnect ((gateway discord-gateway))
  "Disconnect from Discord."
  nil)

(defmethod gateway-send ((gateway discord-gateway)
			 (message message))
  "Send a message on Discord."
  nil)
