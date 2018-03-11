(in-package :common-bot)

(deftype bot-expression ()
  "An expression to be evaluated by a bot."
  `cons)

(defun bot-eval (bot message expression)
  "Evaluate a bot expression in a context."
  (cond ((typep expression 'bot-expression)
	 (command-eval (car expression)
		       bot
		       message
		       (mapcar (lambda (argument)
				 (localize (bot-eval bot message argument)
					   nil nil))
			       (cdr expression))))
	(t expression)))

(defun bot-read (bot object)
  "Read and parse an object into a bot expression."
  ;; to be expanded upon
  (cond ((stringp object)
	 (bot-read-string bot object))
	(t object)))

(defun bot-read-string (bot string)
  "Read and parse a string into a bot expression."
  (let* ((trimmed (string-left-trim " " string))
	 (prefix (message-prefixed-p bot trimmed))
	 (clean (if prefix
		    (subseq trimmed
			    (length prefix))
		    trimmed)))
    (read-string clean)))

(defun message-prefixed-p (bot object)
  "Whether a message is prefixed with an appropriate bot prefix."
  (when (stringp object)
    (find-if (lambda (prefix)
	       (prefixed-p object prefix))
	     (bot-prefixes bot))))
