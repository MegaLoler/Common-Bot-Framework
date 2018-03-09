(in-package :common-gateway)

(defstruct (message (:print-object message-print))
  "Represents a chat message."
  (id nil :type string :read-only t)
  (content nil :type string :read-only t)
  (timestamp nil :type string :read-only t)
  (channel nil :type channel :read-only t)
  (author nil :type user :read-only t))

(defun designates-message-p (string message)
  "Whether a string designates a message."
  (or (string-equal (message-content message)
		    string)
      (string-equal (message-id message)
		    string)))

(defun message-in-channel (string channel)
  "Return a message designated by a string in the context of a channel."
  (find-if (lambda (message)
	     (designates-message-p string message))
	   (channel-messages channel)))

(defun message (designator &optional context)
  "Return a message designated by a designator."
  (cond ((message-p designator)
	 designator)
	((stringp designator)
	 (message-in-channel designator context))
	(t (error "Invalid message designator!"))))

(defun message-server (message)
  "Get the server of a message."
  (channel-server (message-channel message)))

(defun message-gateway (message)
  "Get the gateway of a message."
  (server-gateway (message-server message)))

(defun message-peers (message)
  "Get the users in the same channel as the message."
  (channel-users (message-channel message)))

(defun message-private-p (message)
  "Whether a message occurs in a private channel or not."
  (channel-private-p (message-channel message)))

(defun message-public-p (message)
  "Whether a message occurs in a public channel or not."
  (channel-public-p (message-channel message)))

(defun message-server-p (message)
  "Whether a message is part of a server or not."
  (channel-server-p (message-channel message)))

(defgeneric message-revisions (message)
  (:documentation "Get the revisions/edits of a message.")) ;; return sequence of message objects

(defun message-print (message stream)
  "Print message as a readable string."
  (format stream "~S" (message-id message)))

(defmethod stringify ((message message))
  "Generic user friendly string form for a message object."
  (message-content message))
