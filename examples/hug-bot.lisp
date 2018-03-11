(defpackage :hug-bot
  (:use :cl :common-gateway :lispcord-gateway :common-bot :common-bot.common))
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

(defcommand hug-command
    ((hug give-hug)
     :documentation "Give someone a hug! :heart:"
     :examples '((hug bob) (hug bob linda))
     :permitted #'hug-permitted)
    (bot message &rest recipients)
  recipients)




;; the bot definition itself!
;; including the commands it is to recognize

(defbot hug-bot (commands-command info-command hug-command dummy-command)
    :name "Hug Bot"
    :programmer "Mego#8517"
    :documentation "Just an example bot for testing this bot framework!"
    :prefixes '("$$" "test!")
    :language (make-english-language)
    :personality (make-basic-personality))



;; now connect the bot to some chat services

(defvar *discord*) ;; the gateway

(defun prompt (prompt-string)
  "Prompt the user for a string."
  (format t "~A:~%" prompt-string)
  (read-line))

(defun connect (&optional (discord-token (prompt "Discord Token")))
  "Connect the chat client to the gateways."
  (setf *discord*
	(bot-connect
	 hug-bot
	 `(discord
	   ,discord-token))))

(defun disconnect ()
  "Disconnect the chat client from the gateways."
  (bot-disconnect hug-bot *discord*))

;; (connect)
;; (disconnect)
;; (gateway-send *discord* (prompt "Message") *last-channel*)

;;(command (car (bot-commands hug-bot)) hug-bot)

;;(bot-command-by-alias hug-bot (car (bot-commands hug-bot)))

;;(bot-command-by-alias hug-bot 'info)

;;(localize (commands-string hug-bot 2) nil nil)

