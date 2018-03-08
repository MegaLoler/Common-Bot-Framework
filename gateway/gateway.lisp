(in-package :gateway)

;; abstract chat service interface

(defgeneric gateway-connect ((gateway gateway))
  (:documentation "Connect to the chat service."))

(defgeneric gateway-disconnect ((gateway gateway))
  (:documentation "Disconnect from the chat service."))

(defgeneric gateway-send ((gateway gateway) (message message))
  (:documentation "Send a chat message.")
