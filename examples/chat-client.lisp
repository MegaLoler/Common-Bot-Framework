(defpackage :chat-client
  (:use :cl :gateway :discord-gateway))
(in-package :chat-client)

;; an example chat client for the abstract chat service interface

;; define event handlers

(defevent connect (gateway)
  (format t "Connected to ~A as user ~A."
	  (gateway-name gateway)
	  (gateway-user gateway)))

(defevent disconnect (gateway)
  (format t "Disconnected from ~A."
	  (gateway-name gateway)))

(defevent message (gateway message)
  (if (message-private-p message)
      (format t "[~A] ~A> ~A~%"
	      (gateway-name gateway)
	      (message-author message)
	      (message-content message))
      (format t "[~A] ~A> #~A> ~A~%"
	      (gateway-name gateway)
	      (message-server message)
	      (message-channel message)
	      (message-content message))))

;; connect to the gateways

(defun prompt (prompt-string)
  "Prompt the user for a string."
  (format t "~A: ")
  (read-line))

(defun connect ()
  "Start the chat client."
  (gateway-connect
   (gateway
    `(discord
      ,(prompt "Discord Token: ")))))
