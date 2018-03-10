(defpackage :hug-bot
  (:use :cl :common-bot :common-bot.commands))
(in-package :hug-bot)

;; i want defining bots to be this simple
;; just a bot definition and some command definitions
;; then connect the bot to some chat service(s) with a gateway designator

;; honestly still trying to figure out how i want to commands to work



;; some common commands are available via :bot.commands package
;; but here's a command definition for this bot!

;; a command that takes `recipients' as arguments, and is only permitted to be used among other users
;; and which tells all those recipients they've been hugged

(defun hug-permitted (bot message)
  "Whether the hug command can be used in some context."
  (declare (ignorable bot))
  (message-public-p message))

(defun hug-evaluator (bot message &rest recipients)
  "What the hug command evaluates to when invoked."
  (declare (ignorable bot))
  (cons message recipients))

(defun hug-presenter (values stream)
  "When the results of the hug command is to be displayed."
  (let ((message (car values))
	(recipients (cdr values)))
    (loop
       :for recipient :in recipients
       :do (format stream "_~A gives a big ol' hug to ~A!_ :heart:~%"
		   (message-author message)
		   (user recipient message)))))

(defcommand hug-command (hug give-hug)
  :documentation "Give someone a hug! :heart:"
  :permitted #'hug-permitted
  :evaluator #'hug-evaluator
  :presenter #'hug-presenter)
  



;; the bot definition itself!

(defbot hug-bot (commands-command info-command hug-command)
    :name "Hug Bot"
    :programmer "MegaLoler"
    :documentation "Just an example bot for testing this bot framework!")



;; now connect the bot to some chat services

(defun prompt (prompt-string)
  "Prompt the user for a string."
  (format t "~A:~%" prompt-string)
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
