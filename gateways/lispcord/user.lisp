(in-package :lispcord-gateway)

(defmethod user-in-gateway ((string string) (gateway lispcord-gateway))
  "Return a user designated by a string in the context of a lispcord gateway."
  nil) ;todo

(defmethod discriminable-name ((user lispcord-user))
  "Get a name that can discriminate a user on the gateway (e.g. a Discord tag)."
  (lispcord-user-tag user))

(defmethod local-name ((user lispcord-user) (server lispcord-server))
  "Get the nickname of a user in some context (e.g. a server)."
  "NICK NAME LOL") ;todo, get server nick of user

(defmethod stringify ((user lispcord-user))
  "User friendly string representation of a user in Discord."
  (format nil "<@~A>" (user-id user)))
