(in-package :lispcord-gateway)

(defmethod channel-messages ((channel lispcord-channel))
  "Get the messages in a Discord channel."
  nil) ;todo

(defmethod channel-users ((channel lispcord-channel))
  "Get the users in a Discord channel."
  nil) ;todo

(defmethod channel-private-p ((channel lispcord-channel))
  "Whether a Discord channel is a private (not group) DM channel or not."
  (lispcord-channel-private channel))

(defmethod stringify ((channel lispcord-channel))
  "User friendly string representation of a channel in Discord."
  (format nil "<#~A>" (channel-id channel)))
