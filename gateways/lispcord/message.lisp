(in-package :lispcord-gateway.message)

;; message

(defstruct (lispcord-message (:include message))
  "Represents a Discord chat message."
  (channel nil :type lispcord-channel :read-only t)
  (author nil :type lispcord-user :read-only t))

(defmethod message-revisions ((message lispcord-message))
  nil) ;todo -- return sequence of revisions as message objects

(defmethod stringify ((message lispcord-message))
  "User friendly string representation of a message in Discord."
  (message-content message))
