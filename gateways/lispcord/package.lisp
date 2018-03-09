(defpackage :lispcord-gateway
  (:documentation "Top level package.")
  (:use :cl :common-gateway)
  (:export lispcord-gateway
	   lispcord-gateway-specifier-p
	   discord
	   lispcord-gateway-p
	   lispcord-gateway-token
	   lispcord-gateway-bot
	   make-lispcord-gateway
	   gateway-servers
	   gateway-user
	   lispcord-gateway-init
	   lispcord-gateway-designator
	   lispcord-gateway-print

	   lispcord-server
	   lispcord-server-p
	   make-lispcord-server
	   server-channels
	   server-users

	   lispcord-channel
	   lispcord-channel-p
	   make-lispcord-channel
	   channel-messages
	   channel-users
	   channel-private-p
	   lispcord-channel-private

	   lispcord-message
	   lispcord-message-p
	   make-lispcord-message
	   message-revisions

	   lispcord-user
	   lispcord-user-p
	   lispcord-user-tag
	   make-lispcord-user
	   user-in-gateway
	   user-discriminable-name
	   user-local-name

	   gateway-connect
	   gateway-disconnect
	   gateway-send
	   
	   make-lispcord-bot
	   lispcord-on-ready
	   lispcord-on-close
	   lispcord-on-message

	   stringify))
