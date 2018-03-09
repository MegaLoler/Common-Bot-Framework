(defpackage :chat-client
  (:use :cl :common-gateway :lispcord-gateway))
(in-package :chat-client)

;; an example chat client for the Common Gateway abstract chat service interface

;; define event handlers

(defun on-connect (gateway)
  "When the chat client connects to a gateway."
  (format t "Connected to ~A as user ~A."
	  (gateway-name gateway)
	  (gateway-user gateway)))

(defun on-disconnect (gateway)
  "When the chat client disconnects from a gateway."
  (format t "Disconnected from ~A."
	  (gateway-name gateway)))

(defun on-message (gateway message)
  "When the chat client receives a message from a gateway."
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
  (let ((discord
	 (gateway
	  `(discord
	    ,(prompt "Discord Token: ")))))
    (gateway-add-listener discord :connect #'on-connect)
    (gateway-add-listener discord :disconnect #'on-disconnect)
    (gateway-add-listener discord :message #'on-message)
    (gateway-connect discord)))
