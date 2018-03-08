(in-package :gateway)

;; event definitions

(defmacro defevent (event-name lambda-list &body body)
  "Define an event listener function."
  `(setf (event ',event-name)
	 (lambda ,lambda-list ,@body)))

(deftype event ()
  "An event is just a function in this case."
  'function)

(defun event-p (value)
  "Whether `value' is of type `event'."
  (typep value 'event))

(defun event (event)
  "Get an event designated by `event'."
  (cond ((event-p event) event)
	((symbolp event)
	 (get event 'event))
	(t (error "Invalid event designator!"))))

(defun notify (event &rest args)
  "Trigger an event with arguments."
  (apply (event event) args))
