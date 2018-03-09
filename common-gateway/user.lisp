(in-package :common-gateway)

(defstruct (user (:print-object user-print))
  "Represents a chat user."
  (name nil :type string :read-only t)
  (id nil :type string :read-only t)
  (bot nil :type boolean :read-only t)
  (gateway nil :type gateway :read-only t))
;; todo: add presence; status, message, etc

(defun designates-user-p (string user)
  "Whether a string designates a user."
  (or (string-equal (user-name user)
		    string)
      (string-equal (user-discriminable-name user)
		    string)
      (string-equal (user-local-name user)
		    string)
      (string-equal (user-id user)
		    string)))

(defmethod user-in-context ((string string) (gateway gateway))
  "Return a user designated by a string in the context of a gateway."
  (find-if (lambda (user)
	     (designates-user-p string user))
	   (gateway-users gateway)))

(defmethod user-in-context ((string string) (server server))
  "Return a user designated by a string in the context of a server."
  (find-if (lambda (user)
	     (designates-user-p string user))
	   (server-users server)))

(defmethod user-in-context ((string string) (channel channel))
  "Return a user designated by a string in the context of a channel."
  (find-if (lambda (user)
	     (designates-user-p string user))
	   (channel-users channel)))

(defun user (designator &optional context)
  "Return a user designated by a designator."
  (cond ((user-p designator)
	 designator)
	((stringp designator)
	 (user-in-context designator context))
	(t (error "Invalid user designator!"))))

(defmethod user-discriminable-name ((user user))
  "Get a name that can discriminate a user on the gateway (e.g. a Discord tag)."
  (user-id user))

(defmethod user-local-name ((user user) (context t))
  "Get the nickname of a user in some context (e.g. a server)."
  (user-name user))

(defun user-print (user stream)
  "Print user as a readable string."
  (format stream "~S" (user-discriminable-name user)))

(defmethod stringify ((user user))
  "Generic user friendly string form for a user object."
  (user-name user))
