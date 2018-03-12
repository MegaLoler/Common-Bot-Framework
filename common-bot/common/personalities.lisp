(in-package :common-bot.common)

;; (defpersonality basic-personality
;;     (english "Basic Personality"))

(defstruct
    (basic-personality
      (:include
       personality
       (name (basic-personality-name-string)))))

(deflocalizable basic-personality-name-string ()
  "Localized name of the basic personality."
  "Basic Personality")
