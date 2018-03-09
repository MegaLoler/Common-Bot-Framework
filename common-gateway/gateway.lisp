(in-package :common-gateway)

;; abstract gateway

(defstruct gateway
  "An abstract interface to a chat service."
  (name nil :type string)
  (listeners (make-listeners-table) :type hash-table))

(defmacro def-gateway-specifier (symbol function)
  "Define a gateway specifier as a cons that starts with `symbol' and is resolved by function designator `function' which returns a gateway designated by the gateway specifier."
  `(setf (get ',symbol 'gateway)
	 ',function))

(defun gateway-specifier-p (expression)
  "Whether an expression is a gateway specifier."
  (declare (type cons expression))
  (fboundp
   (functionp
    (get (car expression) 'gateway))))

(defun gateway (designator)
  "Return a gateway designated by a designator."
  (cond ((gateway-p designator)
	 designator)
	((gateway-specifier-p designator)
	 (funcall (get (car designator)
		       'gateway)
		  designator))
	(t (error "Invalid gateway designator!"))))

(defgeneric gateway-servers (gateway)
  (:documentation "Get the servers of a gateway."))

(defmethod gateway-users ((gateway gateway))
  "Get the users of a gateway."
  nil) ;; assuming you can't just grab all users in a gateway by default

(defmethod stringify ((gateway gateway))
  "User friendly string form of a gateway object."
  (gateway-name gateway))

(defun make-listeners-table ()
  "Make a hash table of an array of event listeners for all defined events."
  (let ((listeners (make-hash-table)))
    (loop
       :for event :across *events*
       :do (setf (gethash event listeners)
		 (make-array 0
			     :adjustable t
			     :element-type 'function)))))

(defun gateway-get-listeners (gateway event)
  "Get the event listeners of a gateway for an event."
  (gethash event (gateway-listeners gateway)))

(defun gateway-add-listener (gateway event function)
  "Add an event listener function to a gateway for an event."
  (vector-push-extend function
		      (gateway-get-listeners gateway event)))

;; gateway interface

(defgeneric gateway-connect (gateway)
  (:documentation "Connect to the chat service."))

(defgeneric gateway-disconnect (gateway)
  (:documentation "Disconnect from the chat service."))

(defgeneric gateway-send (gateway string channel)
  (:documentation "Send a chat message."))

;; events

(defevent :connect (gateway)
  (:documentation "When the gateway has connected."))

(defevent :disconnect (gateway)
  (:documentation "When the gateway has disconnected."))

(defevent :message (gateway message)
  (:documentation "When a message has been received from the gateway."))
