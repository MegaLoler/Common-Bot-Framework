(in-package :common-bot)

(defstruct language
  "Represents a natural language."
  (code nil :type symbol :read-only t)
  (name nil :type localizable :read-only t))

(defmacro deflang (name code &rest localization-clauses)
  "Define a natural language."
  (let* ((name-string-name (cat name 'name-string))
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
	   (,(cat name 'language)
	     (:include
	      language
	      (code (localize (,name-string-name) nil nil))
	      (name (,name-string-name)))))
       (deflocalizable ,name-string-name () ,code)
       ,@clauses)))
