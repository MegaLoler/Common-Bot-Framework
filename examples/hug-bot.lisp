(defpackage :hug-bot
  (:use :cl :common-bot :common-bot.commands))
(in-package :hug-bot)

;; i want defining bots to be this simple
;; just a bot definition and some command definitions
;; then connect the bot to some chat service(s) with a gateway designator



;; the bot definition itself!

(defbot hug-bot
    :name "Hug Bot"
    :programmer "MegaLoler"
    :documentation "Just an example bot for testing this bot framework!")



;; some common commands are available via :bot.commands package
;; but here's a command definition for this bot!

;; a command that takes `recipients' as arguments, and is only permitted to be used among other users
;; and which tells all those recipients they've been hugged
(defcommand ((hug give-hug)
	     :documentation "Give someone a hug! :heart:"
	     :permitted-p (lambda (bot message)
			    (among-others-p message)))
    (bot message stream &rest recipients)
  (loop
     :for recipient :in recipients
     :do (format stream "_~A gives a big ol' hug to ~A!_ :heart:~%"
		 (message-author message)
		 (user recipient message))))



;; now connect the bot to some chat services

(defun prompt (prompt-string)
  "Prompt the user for a string."
  (format t "~A: ")
  (read-line))

(defun connect ()
  "Connect and run hug bot!"
  ;; connect the bot to discord!
  (bot-connect
   hug-bot
   `(discord
     ,(prompt "Discord Token: ")))
  
  ;; connect the bot to skype too!
  ;; (bot-connect
  ;;  hug-bot
  ;;  `(skype
  ;;    ,(prompt "Skype username: ")
  ;;    ,(prompt "Skype password: ")))
  )
