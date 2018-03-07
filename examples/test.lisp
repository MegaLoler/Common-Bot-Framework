(defpackage :bot-test
  (:use :cl :bot :bot.commands))
(in-package :bot-test)

(defbot test-bot
    :name "Test Bot"
    :author "MegaLoler"
    :documentation "Just an example bot for testing my Common Lisp bot framework!"
    :prefix ("!" "."))
