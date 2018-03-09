(in-package :common-gateway)

(defstruct (server (:print-object server-print))
  "Represents a chat server."
  (name nil :type string :read-only t)
  (id nil :type string :read-only t)
  (gateway nil :type gateway :read-only t))

(defun designates-server-p (string server)
  "Whether a string designates a server."
  (or (string-equal (server-name server)
		    string)
      (string-equal (server-id server)
		    string)))

(defun server-in-gateway (string gateway)
  "Return a server designated by a string in the context of a gateway."
  (find-if (lambda (server)
	     (designates-server-p string server))
	   (gateway-servers gateway)))

(defun server (designator &optional context)
  "Return a server designated by a designator."
  (cond ((server-p designator)
	 designator)
	((stringp designator)
	 (server-in-gateway designator context))
	(t (error "Invalid server designator!"))))

(defgeneric server-channels (channel)
  (:documentation "Get the messages in a server."))

(defgeneric server-users (channel)
  (:documentation "Get the users in a server."))

(defun server-print (server stream)
  "Print server as a readable string."
  (format stream "~S" (server-id server)))

(defmethod stringify ((server server))
  "Generic user friendly string form for a server object."
  (server-name server))
