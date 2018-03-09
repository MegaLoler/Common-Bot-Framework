(in-package :lispcord-gateway)

(defmethod server-channels ((server lispcord-server))
  "Get the channels in a Discord server."
  nil) ;todo

(defmethod server-users ((server lispcord-server))
  "Get the users in a Discord server."
  nil) ;todo

(defmethod stringify ((server server))
  "User friendly string representation of a server in Discord."
  (format nil "**~A**" (server-name server))) ;; maybe change to a invite link?
