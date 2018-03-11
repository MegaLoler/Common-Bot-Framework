(defpackage :common-gateway
  (:documentation "Top level package.")
  (:use :cl)
  (:export defevent
	   event-notify

	   gateway
	   make-gateway
	   gateway-p
	   gateway-name
	   gateway-listeners
	   def-gateway-specifier
	   gateway-specifier-p
	   gateway-servers
	   gateway-users
	   gateway-user
	   stringify
	   make-listeners-table
	   gateway-get-listeners
	   gateway-add-listener
	   gateway-connect
	   gateway-disconnect
	   gateway-send

	   server
	   make-server
	   server-p
	   server-name
	   server-id
	   server-gateway
	   designates-server-p
	   server-in-gateway
	   server-channels
	   server-users
	   server-print

	   channel
	   make-channel
	   channel-p
	   channel-name
	   channel-id
	   channel-server
	   designates-channel-p
	   channel-in-server
	   channel-gateway
	   channel-messages
	   channel-users
	   channel-private-p
	   channel-public-p
	   channel-server-p
	   channel-print

	   message
	   make-message
	   message-p
	   message-id
	   message-content
	   message-timestamp
	   message-channel
	   message-author
	   designates-message-p
	   message-in-channel
	   message-server
	   message-gateway
	   message-peers
	   message-private-p
	   message-public-p
	   message-server-p
	   message-revisions
	   message-print

	   user
	   make-user
	   user-p
	   user-name
	   user-id
	   user-botp
	   user-gateway
	   designates-user-p
	   user-in-context
	   user-discriminable-name
	   user-local-name
	   user-print

	   stringify))
