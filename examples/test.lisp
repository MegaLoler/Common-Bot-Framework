(defpackage :bot-test
  (:use :cl :bot :bot.commands))
(in-package :bot-test)

;; i want defining bots to be this simple
;; just a bot definition and some command definitions
;; here, common commands are reused from the :bot.commands package
;; and then just plug them into gateways to chat services to connect it to
;; and start with (bot-connect bot)

(defbot test-bot
    :name "Test Bot"
    :author "MegaLoler"
    :documentation "Just an example bot for testing my Common Lisp bot framework!"
    :prefix ("!" "."))

;; TODO: a piece of code for designating gateways to connect to
;; ie, Discord
;; à la `:gateway (discord token)'

;; start the bot!
;; (bot-connect test-bot)
