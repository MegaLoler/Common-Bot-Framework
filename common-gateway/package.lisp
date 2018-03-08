(defpackage :common-gateway
  (:documentation "Top level package.")
  (:use :cl
	:common-gateway.gateway
	:common-gateway.event
	:common-gateway.channel
	:common-gateway.message
	:common-gateway.user))

(defpackage :common-gateway.gateway
  (:documentation "Abstract chat service interface.")
  (:use :cl))

(defpackage :common-gateway.event
  (:documentation "Event and listener definitions.")
  (:use :cl))

(defpackage :common-gateway.channel
  (:documentation "Chat channel objects.")
  (:use :cl))

(defpackage :common-gateway.message
  (:documentation "Chat message objects.")
  (:use :cl))

(defpackage :common-gateway.user
  (:documentation "Chat user objects.")
  (:use :cl))
