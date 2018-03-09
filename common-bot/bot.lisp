(in-package :common-bot)

(defmacro defbot (symbol &rest options)
  "Define a bot and assign it to the bot slot of a symbol."
  `(setf (bot ',symbol)
	 (make-bot ,@options)))

(defstruct bot
  "Represents a bot."
  (name "Common Bot" :type string)
  (programmer "Unknown" :type string)
  (documentation "A bot fresh out of the bot factory." :type string))

(defun bot (bot)
  "Get a bot designated by `bot'."
  (cond ((bot-p bot) bot)
	((symbolp bot)
	 (get bot 'bot))
	(t (error "Invalid bot designator!"))))

(defun bot-commands (bot)
  "Return a list of defined commands visible to a bot."
  (let (commands)
    (do-symbols (symbol)
      (let ((command (command symbol)))
	(if (command-p command)
	    (push command commands))))))

(defun bot-known-command-p (bot command)
  "Whether `bot' recognizes `command'."
  (position (command command) (bot-commands bot)))

(defun bot-connect (bot gateway)
  "Connect a bot to a gateway."
  (gateway-connect (gateway gateway)))

(defevent message (gateway message)
  (channel-send message (bot-eval message)))
