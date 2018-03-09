(in-package :common-gateway)

(defstruct (channel (:print-object channel-print))
  "Represents a chat channel."
  (name nil :type string :read-only t)
  (id nil :type string :read-only t)
  (server nil :type (or null server) :read-only t))

(defun designates-channel-p (string channel)
  "Whether a string designates a channel."
  (or (string-equal (channel-name channel)
		    string)
      (string-equal (channel-id channel)
		    string)))

(defun channel-in-server (string server)
  "Return a channel designated by a string in the context of a server."
  (find-if (lambda (channel)
	     (designates-channel-p string channel))
	   (server-channels server)))

(defun channel (designator &optional context)
  "Return a channel designated by a designator."
  (cond ((channel-p designator)
	 designator)
	((stringp designator)
	 (channel-in-server designator context))
	(t (error "Invalid channel designator!"))))

(defun channel-gateway (channel)
  "Get the gateway of a channel."
  (server-gateway (channel-server channel)))

(defgeneric channel-messages (channel)
  (:documentation "Get the messages in a channel."))

(defgeneric channel-users (channel)
  (:documentation "Get the users in a channel."))

(defgeneric channel-private-p (channel)
  (:documentation "Whether a channel is one-on-one or not."))

(defun channel-public-p (channel)
  "Whether a channel is a group or not."
  (not (channel-private-p channel)))

(defun channel-server-p (channel)
  "Whether a channel is part of a server or not."
  (channel-server channel))

(defun channel-print (channel stream)
  "Print channel as a readable string."
  (format stream "~S" (channel-id channel)))

(defmethod stringify ((channel channel))
  "Generic user friendly string form for a channel object."
  (channel-name channel))
