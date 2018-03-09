(in-package :common-bot.commands)

;; some common bot commands that a lot of bots might find useful

(defcommand ((commands command command-list show-commands)
	     :documentation "See a listing of all the commands you can use with this bot.")
    (bot message stream)
  (format stream "**Commands:**~%")
  (loop
     :for command :in (bot-commands bot)
     :do (when (command-allowed command bot message)
	   (format stream "• ~A~%"
		   (command-description command)))))

(defcomand ((info information credits programmer)
	    :documentation "See some basic information about this bot.")
    (bot message stream)
  (format stream "This is **~A**~%~A~%~%Made by **~A**."
	  (bot-name bot)
	  (bot-documentation bot)
	  (bot-programmer bot)))
