(in-package :lispcord-gateway)

;; lispcord gateway

(defun lispcord-gateway-specifier-p (expression)
  "Whether an expression is a lispcord gateway specifier."
  (declare (type cons expression))
  (equalp (car expression) 'discord))

(defun lispcord-gateway (designator)
  "Return a lispcord gateway designated by a designator."
  (cond ((lispcord-gateway-p designator)
	 designator)
	((lispcord-gateway-specifier-p designator)
	 (make-lispcord-gateway :token (second designator)))
	(t (error "Invalid lispcord gateway designator!"))))

(def-gateway-specifier discord lispcord-gateway)

(defmethod gateway-servers ((gateway lispcord-gateway))
  "Get the servers available to the Discord gateway."
  nil) ;todo

(defmethod gateway-user ((gateway lispcord-gateway))
  "Get the Discord user account used to connect to Discord."
  (lispcord-gateway-me gateway))

(defun lispcord-gateway-init (gateway)
  "Initialize a lispcord gateway object."
  (setf (lispcord-gateway-bot gateway)
	(make-lispcord-bot gateway)))

(defun lispcord-gateway-designator (gateway)
  "Return an expression that designates a lispcord gateway object."
  `(discord ,(lispcord-gateway-token gateway)))

(defun lispcord-gateway-print (gateway stream)
  "Print a lispcord gateway object as a readable string."
  (format stream "~S" (lispcord-gateway-designator gateway)))



;; gateway interface

(defmethod gateway-connect ((gateway lispcord-gateway))
  "Connect to Discord."
  (lispcord-gateway-init gateway)
  (lispcord:connect (lispcord-gateway-bot gateway)))

(defmethod gateway-disconnect ((gateway lispcord-gateway))
  "Disconnect from Discord."
  (lispcord:disconnect (lispcord-gateway-bot gateway)))

;; currently not working in dms; lispcord isnt attaching a channel to received dm messages ?
(defmethod gateway-send ((gateway lispcord-gateway)
			 (string string)
			 (channel lispcord-channel))
  "Send a message on Discord."
  (lispcord:create string
		   (lispcord:from-id (channel-id channel)
				     :channel)
		   (lispcord-gateway-bot gateway)))



;; lispcord

;; quiet the lispcord logger
(lispcord.util:set-log-level :error)

(defun make-lispcord-bot (gateway)
  "Setup a lispcord connection."
  ;; maybe try out the new event handler api instead
  (lispcord.pipes::watch (lambda (ready-payload)
			   (setf (lispcord-gateway-me gateway)
				 (make-gateway-user-from-lispcord-user
				  gateway
				  (lc:user ready-payload)))
			   (lispcord-on-ready gateway))
			 lispcord:>status-ready>)
  (lispcord.pipes::watch (lambda (payload _) ;; not sure what these args are
			   (declare (ignorable payload _))
			   (lispcord-on-close gateway))
			 lispcord:>status-close>)
  (lispcord.pipes::watch (lambda (payload)
			   (lispcord-on-message gateway payload))
			 lispcord:>message-create>)
  (lispcord::make-bot (lispcord-gateway-token gateway)))

(defmethod lispcord-on-ready ((gateway lispcord-gateway))
  "Pass ready events from lispcord to common gateway."
  (event-notify gateway :connect))

(defmethod lispcord-on-close ((gateway lispcord-gateway))
  "Pass close events from lispcord to common gateway."
  (event-notify gateway :disconnect))

(defmethod lispcord-on-message ((gateway lispcord-gateway)
				(message lc:message))
  "Pass message events from lispcord to common gateway."
  (event-notify gateway
		:message
		(make-gateway-message-from-lispcord-message
		 gateway
		 message)))

(defun make-gateway-server-from-lispcord-guild (gateway guild)
  "Make a common gateway server object from a lispcord guild instance."
  (and guild
       (make-lispcord-server :name (if (lc:availablep guild)
				       (lc:name guild)
				       "N/A")
			     :id (lc:id guild)
			     :gateway gateway)))

(defun make-gateway-channel-from-lispcord-channel (gateway channel)
  "Make a common gateway channel object from a lispcord user instance."
  (make-lispcord-channel :name (lc:name channel)
			 :id (lc:id channel)
			 :server (make-gateway-server-from-lispcord-guild
				  gateway
				  (lc:guild channel))))

(defun make-gateway-channel-from-lispcord-message (gateway message)
  "Make a common gateway channel (maybe private) from a lispcord message instance."
  ;; i think this is a bug in lispcord, channel being nil if its not in a guild
  (if (lc:channel message)
      (make-gateway-channel-from-lispcord-channel
       gateway
       (lc:channel message))
      (make-lispcord-channel :name (lc:name (lc:author message))
			     :id (lc:id (lc:author message))
			     :private t)))

(defun make-gateway-user-from-lispcord-user (gateway user)
  "Make a common gateway user object from a lispcord user instance."
  (make-lispcord-user :name (lc:name user)
		      :id (lc:id user)
		      :tag (format nil "~A#~A"
				   (lc:name user)
				   (lc:discrim user))
		      :bot (lc:botp user)
		      :gateway gateway))

(defun make-gateway-message-from-lispcord-message (gateway message)
  "Make a common gateway message object from a lispcord message instance."
  (make-lispcord-message :id (lc:id message)
			 :content (lc:content message)
			 :timestamp (lc:timestamp message)
			 :channel (make-gateway-channel-from-lispcord-message
				   gateway
				   message)
			 :author (make-gateway-user-from-lispcord-user
				  gateway
				  (lc:author message))))

(defun get-member-by-id (id guild)
  "Get a lispcord member instance by user id from a lispcord guild instance."
  (and guild
       (lc:availablep guild)
       (find-if (lambda (member)
		  (string-equal (lc:id
				 (lc:user member))
				id))
		(lc:members guild))))

(defun get-guild-by-id (id)
  "Get a lispcord guild instance by id."
  (lc::getcache-id id :guild))

(defun get-lispcord-guild-from-gateway-server (server)
  "Get a lispcord guild instance from a gateway server object."
  (get-guild-by-id (server-id server)))
