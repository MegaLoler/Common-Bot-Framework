(in-package :bot.commands)

(defcommand ((commands command command-list show-commands)
	     :documentation "See a listing of all the commands you can use with this bot.")
    (bot message stream)
  (format stream "**Commands:**~%")
  (loop
     :for command :in (bot-commands bot)
     :do (when (command-allowed command bot message)
	   (format stream "• ~A~%"
		   (command-description command)))))

(defcommand ((hug give-hug)
	     :documentation "Give someone a hug! :heart:"
	     :permitted-p (lambda (bot message)
			    (among-others-p message)))
    (bot message stream &rest recipients)
  (loop
     :for recipient :in recipients
     :do (format stream "_~A gives a big ol' hug to ~A!_ :heart:~%"
		 (to-string (user message))
		 (to-string (user recipient message)))))
