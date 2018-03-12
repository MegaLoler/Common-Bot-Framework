(in-package :common-bot.common)

;; documentation strings

(deflocalizable documentation-unavailable-string ()
  "Message when a command has no documentation."
  nil)

(deflocalization (documentation-unavailable-string english nil) ()
  "English message saying a command has no documentation."
  "Documentation unavailable.")

(deflocalizable info-command-documentation ()
  "Documentation of the info command."
  :info-command-documentation)

(deflocalization (info-command-documentation english nil) ()
  "Documentation of the info command in English."
  "See some basic information about this bot.")

(deflocalizable commands-command-documentation ()
  "Documentation of the commands command."
  :commands-command-documentation)

(deflocalization (commands-command-documentation english nil) ()
  "Documentation of the commands command in English."
  "See a listing of all the commands you can use with this bot.")



;; command result strings

(deflocalizable info-string (bot message)
  "String to display bot information."
  bot)

(deflocalization (info-string english nil) (bot message)
  "Display bot info in English."
  (fmt:concat "This is " (fmt:bold (bot-name bot))
	      #\Newline
	      (bot-documentation bot)
  	      #\Newline #\Newline
  	      "Made with " (emoji:heart)
  	      " by " (or (user (bot-programmer bot) message)
  			 (bot-programmer bot)) "."
  			 #\Newline
  			 "Written in " (fmt:bold "Common Lisp")
  			 " using " (fmt:italic "Common Bot Framework") "."))

(deflocalization (info-string dutch nil) (bot message)
  "Display bot info in Dutch."
  (fmt:concat "Dit is " (fmt:bold (bot-name bot))
	      #\Newline
	      (bot-documentation bot)
  	      #\Newline #\Newline
  	      "Gemaakt met " (emoji:heart)
  	      " door " (or (user (bot-programmer bot) message)
  			   (bot-programmer bot)) "."
  			   #\Newline
  			   "Geschreven in " (fmt:bold "Common Lisp")
  			   " met gebruik van " (fmt:italic "Common Bot Framework") "."))

(deflocalizable commands-string (bot message)
  "String to display command listing."
  (bot-commands bot))

;; this needs much better formatting lol
(deflocalization (commands-string english nil) (bot message)
  "Display command listing in English."
  (let ((lines
	 (loop
	    :for command :in (bot-commands bot)
	    :when (command-permitted-p command bot message)
	    :collect (fmt:concat
		      "• " (fmt:code command) #\Newline
		      (fmt:join (command-aliases command))
		      #\Newline
		      "› " (or (localize (command-documentation command)
					 (bot-language bot)
					 (bot-personality bot))
			       (fmt:italic
				(localize (documentation-unavailable-string)
					  (bot-language bot)
					  (bot-personality bot))))
		      #\Newline))))
    (fmt:concat (fmt:bold "Commands:")
		#\Newline #\Newline
		(fmt:join lines #\Newline))))
