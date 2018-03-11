(in-package :common-bot.common)

;; make filterable with given args
(defcommand commands-command
    ((commands command command-list command-listing list-commands show-commands display-commands help)
     :documentation "See a listing of all the commands you can use with this bot.")
    (bot message)
  (commands-string bot message))

;; make potentially able to get info on Other bots
(defcommand info-command
    ((bot info information credits programmer about)
     :documentation "See some basic information about this bot.")
    (bot message)
  (info-string bot message))

(defcommand dummy-command (dummy)
    (bot message)
  "dummy")
