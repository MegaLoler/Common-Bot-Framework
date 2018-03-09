(defpackage :lispcord-gateway
  (:documentation "Top level package.")
  (:use :cl
	:lispcord-gateway.gateway
	:lispcord-gateway.server
	:lispcord-gateway.channel
	:lispcord-gateway.message
	:lispcord-gateway.user))

(defpackage :lispcord-gateway.gateway
  (:documentation "Interface to Discord via Lispcord.")
  (:use :cl))

(defpackage :lispcord-gateway.server
  (:documentation "Chat server objects.")
  (:use :cl))

(defpackage :lispcord-gateway.channel
  (:documentation "Chat channel objects.")
  (:use :cl))

(defpackage :lispcord-gateway.message
  (:documentation "Chat message objects.")
  (:use :cl))

(defpackage :lispcord-gateway.user
  (:documentation "Chat user objects.")
  (:use :cl))
