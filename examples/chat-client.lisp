(defpackage :chat-client
  (:documentation "An example chat client for the Common Gateway abstract chat service interface.")
  (:use :cl :common-gateway :lispcord-gateway))
(in-package :chat-client)

;; the gateways

(defvar *discord*)

;; the last channel where a message was received

(defvar *last-channel*)

;; define event handlers

(defun on-connect (gateway)
  "When the chat client connects to a gateway."
  (format t "Connected to ~A as user ~A.~%"
	  (gateway-name gateway)
	  (user-discriminable-name (gateway-user gateway))))

(defun on-disconnect (gateway)
  "When the chat client disconnects from a gateway."
  (format t "Disconnected from ~A.~%"
	  (gateway-name gateway)))

(defun on-message (gateway message)
  "When the chat client receives a message from a gateway."
  (setf *last-channel* (message-channel message))
  (if (message-private-p message)
      (format t "[~A] ~A> ~A~%" ;; private dm
	      (gateway-name gateway)
	      (user-discriminable-name (message-author message))
	      (message-content message))
      (if (message-server-p message)
	  (format t "[~A] ~A> #~A> ~A (~A)> ~A~%" ;; server
		  (gateway-name gateway)
		  (server-name (message-server message))
		  (channel-name (message-channel message))
		  (user-local-name (message-author message)
				   (message-server message))
		  (user-discriminable-name (message-author message))
		  (message-content message))
	  (format t "[~A] #~A> ~A> ~A~%" ;; group dm
		  (gateway-name gateway)
		  (channel-name (message-channel message))
		  (user-discriminable-name (message-author message))
		  (message-content message)))))

;; connect to the gateways

(defun prompt (prompt-string)
  "Prompt the user for a string."
  (format t "~A:~%" prompt-string)
  (read-line))

(defun init (&optional (discord-token (prompt "Discord Token")))
  "Initialize the chat client."
  (setf *discord* (gateway
		   `(discord
		   ,discord-token)))
  (gateway-add-listener *discord* :connect #'on-connect)
  (gateway-add-listener *discord* :disconnect #'on-disconnect)
  (gateway-add-listener *discord* :message #'on-message))

(defun connect ()
  "Connect the chat client to the gateways."
  (gateway-connect *discord*))

(defun disconnect ()
  "Disconnect the chat client from the gateways."
  (gateway-disconnect *discord*))

;; (init)
;; (connect)
;; (disconnect)
;; (gateway-send *discord* (prompt "Message") *last-channel*) ;; currently failing in dms
