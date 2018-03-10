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
