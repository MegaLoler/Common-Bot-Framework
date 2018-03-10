(in-package :common-bot)

(defun cat (prefix suffix)
  "Concatenate symbols."
  (read-from-string
   (format nil "~A-~A"
	   prefix
	   suffix)))
