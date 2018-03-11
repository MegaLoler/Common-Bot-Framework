(in-package :lispcord-gateway)

(defmethod server-channels ((server lispcord-server))
  "Get the channels in a Discord server."
  nil) ;todo

(defmethod server-users ((server lispcord-server))
  "Get the users in a Discord server."
  (loop
     :for member :across (lc:members
			  (get-lispcord-guild-from-gateway-server
			   server))
     :collect (make-gateway-user-from-lispcord-user
	       (server-gateway server)
	       (lc:user member))))

(defmethod stringify ((server server))
  "User friendly string representation of a server in Discord."
  (format nil "**~A**" (server-name server))) ;; maybe change to a invite link?
