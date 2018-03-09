(in-package :lispcord-gateway)

(defmethod message-revisions ((message lispcord-message))
  nil) ;todo -- return sequence of revisions as message objects

(defmethod stringify ((message lispcord-message))
  "User friendly string representation of a message in Discord."
  (message-content message))
