(in-package :common-bot.commands)

;; command to see all known commands of a bot

(defun commands-evaluator (bot message) ;; later make it filterable via args
  "Return the commands to be listed."
  (declare (ignorable message))
  (cons bot (bot-commands bot)))

(defun commands-presenter (values stream) ;; use embeds yo !! generalized rich text!!
  "Display the commands."
  (let ((bot (car values))
	(commands (cdr values)))
    (format stream "**Commands:**~%")
    (loop
       :for command :in commands
       :do (when (bot-command-p bot command)
	     (format stream "• ~A~%"
		     (command-documentation command))))))

(defcommand commands-command (commands command command-list command-listing list-commands show-commands display-commands help)
  :documentation "See a listing of all the commands you can use with this bot."
  :evaluator #'commands-evaluator
  :presenter #'commands-presenter)

;; command to see general info about a bot

(defun info-evaluator (bot message) ;; potentially implement bot designators and get information on other bots given as args!!
  "Return the bot to get info on."
  (declare (ignorable message))
  bot)

(defun info-presenter (bot stream)
  "Display the information."
  (format stream "This is **~A**~%~A~%~%Made with :heart: by **~A**.~%Written in **Common Lisp** using **Common Bot Framework**."
	  (bot-name bot)
	  (bot-documentation bot)
	  (bot-programmer bot)))

(defcommand info-command (info information credits programmer about)
  :documentation "See some basic information about this bot."
  :evaluator #'info-evaluator
  :presenter #'info-presenter)
