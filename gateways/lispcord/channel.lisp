(in-package :lispcord-gateway)

(defstruct (lispcord-channel (:include channel))
  "Represents a Discord chat channel."
  (server nil :type lispcord-server :read-only t))

(defmethod channel-messages ((channel lispcord-channel))
  "Get the messages in a Discord channel."
  nil) ;todo

(defmethod channel-users ((channel lispcord-channel))
  "Get the users in a Discord channel."
  nil) ;todo

(defmethod stringify ((channel lispcord-channel))
  "User friendly string representation of a channel in Discord."
  (format nil "<#~A>" (channel-id channel)))
