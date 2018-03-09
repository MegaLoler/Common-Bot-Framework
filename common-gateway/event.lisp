(in-package :common-gateway.event)

;; event definitions

(defvar *events* (make-array 0
			     :adjustable t
			     :element-type 'symbol))

(defmacro defevent (event lambda-list options)
  "Define a recognized event for a gateway."
  (declare (ignorable lambda-list options)) ;; for now
  (vector-push-extend event *events*))

(defun event-notify (gateway event &rest arguments)
  "Notify event listeners of a gateway of an event with arguments."
  (loop
     :for listener :across (gateway-get-listeners gateway event)
     :do (apply listener (cons gateway arguments))))

;; todo: instead of globally recognized events, make subclasses of gateways able to extend the known events

;; todo: make event struct to store documentation and lambda list approprate for handlers of the event

;; todo: event-listener struct and deflistener that ensures functions are appropriate for the event
