(in-package :common-bot.common)

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
