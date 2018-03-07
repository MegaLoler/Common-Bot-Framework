(in-package :bot)

(defmacro defbot (symbol &rest options)
  "Define a bot and assign it to the bot slot of a symbol."
  `(setf (bot ,symbol) (make-bot ,@options)))

(defstruct bot
  "Represents a bot."
  (name "Common Bot" :type string)
  (author "Unknown" :type string)
  (documentation "A bot fresh out of the bot factory." :type string)
  (prefix nil)
  (expression-reader #'default-bot-expression-reader :type function)
  (expression-printer #'default-bot-expression-printer :type function))

(defun bot (bot)
  "Get a bot designated by `bot'."
  (if (bot-p bot)
      bot
      (get bot 'bot)))

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

(defun bot-reasonable-prefix (bot)
  "Get a reasonable prefix for use with a bot or nil."
  (let ((prefix (bot-prefix bot)))
    (if (listp prefix)
	(car prefix)
	prefix)))

(defun bot-reasonable-prefix-string (bot)
  "Get a reasonable prefix for use with a bot or empty string."
  (or (bot-reasonable-prefix bot) ""))
