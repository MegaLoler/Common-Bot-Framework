(in-package :common-bot.common)

;; !! do this better so theres no style warnings !!
;; probably use clos instead of structs?

(deflang english :en
  (english "English")
  (dutch "Engels")
  (german "Englisch"))

(deflang dutch :nl
  (english "Dutch")
  (dutch "Nederlands")
  (german "Niederdeutsch"))

(deflang german :de
  (english "German")
  (dutch "Duits")
  (german "Deutsch"))
