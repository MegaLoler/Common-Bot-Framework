(in-package :lispcord-gateway)

(defmethod user-in-gateway ((string string) (gateway lispcord-gateway))
  "Return a user designated by a string in the context of a lispcord gateway."
  nil) ;todo

(defmethod user-discriminable-name ((user lispcord-user))
  "Get a name that can discriminate a user on the gateway (e.g. a Discord tag)."
  (lispcord-user-tag user))

(defmethod user-local-name ((user lispcord-user) (server lispcord-server))
  "Get the nickname of a user in some context (e.g. a server)."
  (let ((member (get-member-by-id
		 (user-id user)
		 (get-lispcord-guild-from-gateway-server
		  server))))
    (or (and member
	     (lc:nick member))
	(user-name user))))

(defmethod stringify ((user lispcord-user))
  "User friendly string representation of a user in Discord."
  (format nil "<@~A>" (user-id user)))
