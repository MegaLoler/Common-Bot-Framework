

;; user

(defstruct (lispcord-user (:include user))
  "Represents a Discord chat user."
  (tag nil :type string :read-only t)
  (gateway nil :type lispcord-gateway :read-only t))

(defmethod discriminable-name ((user lispcord-user))
  "Get a name that can discriminate a user on the gateway (e.g. a Discord tag)."
  (lispcord-user-tag user))

(defmethod local-name ((user lispcord-user) (server lispcord-server))
  "Get the nickname of a user in some context (e.g. a server)."
  nil) ;todo, get server nick of user

(defmethod stringify ((user lispcord-user))
  "User friendly string representation of a user in Discord."
  (format nil "<@~A>" (user-id user))
