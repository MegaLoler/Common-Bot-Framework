(in-package :common-bot)

(defstruct bot
  "Represents a bot."
  (name "Common Bot" :type string)
  (programmer "Unknown" :type string)
  (documentation "A bot fresh out of the bot factory." :type string)
  (prefixes nil :type list)
  (commands nil :type list))

(defmacro defbot (name commands &rest options)
  "Define a bot that knows some commands and assign it as a global variable."
  `(defparameter ,name
     (make-bot :commands (list ,@commands)
	       ,@options)))

(defun bot-command-p (bot command)
  "Whether `bot' recognizes `command'."
  (find (command command bot)
	(bot-commands bot)))

(defun bot-command-by-alias (bot alias)
  "Get a command object known to a bot by an alias."
  (or (find-if (lambda (command)
		 (find-if (lambda (alias2)
			    (string= alias alias2))
			  (command-aliases command)))
	       (bot-commands bot))
      (error "Unrecognized command!")))
