(in-package :gateway)

;; abstract chat service interface

(defgeneric gateway-connect (gateway)
  (:documentation "Connect to the chat service."))

(defgeneric gateway-disconnect (gateway)
  (:documentation "Disconnect from the chat service."))

(defgeneric gateway-send (gateway message)
  (:documentation "Send a chat message."))

;; events

(defevent connect (gateway)
  (:documentation "When the gateway has connected."))

(defevent disconnect (gateway)
  (:documentation "When the gateway has disconnected."))

(defevent message (gateway message)
  (:documentation "When a message has been received from the gateway."))
