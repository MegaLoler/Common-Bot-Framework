(in-package :common-bot.common)

;; make filterable with given args
(defcommand commands-command
    ((commands command command-list command-listing list-commands show-commands display-commands help)
     :documentation (commands-command-documentation))
    (bot message)
  (commands-string bot message))

;; make potentially able to get info on Other bots
(defcommand info-command
    ((bot info information credits programmer about)
     :documentation (info-command-documentation))
    (bot message)
  (info-string bot message))
