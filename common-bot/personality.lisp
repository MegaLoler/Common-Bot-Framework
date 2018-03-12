(in-package :common-bot)

(defstruct personality
  "Represents a bot personality."
  (name nil :type localizable :read-only t))

(defmacro defpersonality (name &rest localization-clauses)
  "Define a bot personality."
  (let* ((name-string-name (cat name 'personality-name-string))
	 (clauses
	  (loop
	     :for localization-clause :in localization-clauses
	     :for language = (first localization-clause)
	     :for localized-name = (second localization-clause)
	     :collect `(deflocalization
			   (,name-string-name ,language nil)
			   () ,localized-name))))
    `(progn
       (defstruct
	   (,(cat name 'personality)
	     (:include
	      personality
	      (name (,name-string-name)))))
       (deflocalizable ,name-string-name () ',name)
       ,@clauses)))
