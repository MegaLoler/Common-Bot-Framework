(in-package :common-bot.commands)

;; command to see all known commands of a bot

;; make filterable with given args
(defcommand commands-command
    ((commands command command-list command-listing list-commands show-commands display-commands help)
     :documentation "See a listing of all the commands you can use with this bot.")
    (bot message)
  (bot-commands bot))

;; command to see general info about a bot

;; make potentially able to get info on Other bots
(defcommand info-command
    ((bot info information credits programmer about)
     :documentation "See some basic information about this bot.")
    (bot message)
  bot)

;; dummy command

(defcommand dummy-command (dummy)
    (bot message)
  "dummy")
