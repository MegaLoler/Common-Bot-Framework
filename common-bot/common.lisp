(in-package :common-bot.common)

;; some language definitions

(defstruct
    (english-language
      (:include
       language
       (code (localize (english-name-string) nil nil))
       (name (english-name-string)))))

(defstruct
    (dutch-language
      (:include
       language
       (code (localize (dutch-name-string) nil nil))
       (name (dutch-name-string)))))

(deflocalizable english-name-string ()
  "Localized name of the English language."
  :en)

(deflocalization (english-name-string english nil) ()
  "English language name in English."
  "English")

(deflocalization (english-name-string dutch nil) ()
  "English language name in Dutch."
  "Engels")

(deflocalizable dutch-name-string ()
  "Localized name of the Dutch language."
  :nl)

(deflocalization (dutch-name-string english nil) ()
  "Dutch language name in English."
  "Dutch")

(deflocalization (dutch-name-string dutch nil) ()
  "Dutch language name in Dutch."
  "Nederlands")



;; some localizable bot strings

(deflocalizable info-string (bot message)
  "String to display bot information."
  bot)

(deflocalization (info-string english nil) (bot message)
  "Display bot info in English."
  "english info lol") ;todo 4 real
  ;; (fmt:concat "This is " (fmt:bold (bot-name bot))
  ;; 	      #\Newline #\Newline
  ;; 	      "Made with " (emoji:heart)
  ;; 	      " by " (or (user (bot-programmer bot) message)
  ;; 			 (bot-programmer bot)) "."
  ;; 			 #\Newline
  ;; 			 "Written in " (fmt:bold "Common Lisp")
  ;; 			 " using " (fmt:italic "Common Bot Framework") "."))

(deflocalization (info-string dutch nil) (bot message)
  "Display bot info in Dutch."
  "nederlandse info lol") ;todo 4 real
  ;; (fmt:concat "Dit is " (fmt:bold (bot-name bot))
  ;; 	      #\Newline #\Newline
  ;; 	      "Gemaakt met " (emoji:heart)
  ;; 	      " door " (or (user (bot-programmer bot) message)
  ;; 			   (bot-programmer bot)) "."
  ;; 			   #\Newline
  ;; 			   "Geschreven in " (fmt:bold "Common Lisp")
  ;; 			   " met gebruik van " (fmt:italic "Common Bot Framework") "."))

(deflocalizable commands-string (bot message)
  "String to display command listing."
  (bot-commands bot))



;; common commands

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
