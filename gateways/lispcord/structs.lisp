(in-package :lispcord-gateway)

(defstruct
    (lispcord-gateway
      (:include gateway
		(name "Discord"))
      (:print-object lispcord-gateway-print))
  "Discord gateway via lispcord."
  (token nil :type string :read-only t)
  (bot nil :type (or null lispcord::bot)))

(defstruct
    (lispcord-server
      (:include server
		(gateway nil
			 :type lispcord-gateway
			 :read-only t)))
  "Represents a Discord chat server.")

(defstruct
    (lispcord-channel
      (:include channel
		(server nil
			:type (or null lispcord-server)
			:read-only t)))
  "Represents a Discord chat channel."
  (private nil :type boolean :read-only t))

(defstruct
    (lispcord-message
      (:include message
		(channel nil
			 :type lispcord-channel
			 :read-only t)
		(author nil
			:type lispcord-user
			:read-only t)))
  "Represents a Discord chat message.")

(defstruct
    (lispcord-user
      (:include user
		(gateway nil
			 :type lispcord-gateway
			 :read-only t)))
  "Represents a Discord chat user."
  (tag nil :type string :read-only t))
