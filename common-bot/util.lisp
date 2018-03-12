(in-package :common-bot)

(defun cat (&rest symbols)
  "Concatenate symbols."
  (read-from-string
   (format nil "~{~A~^-~}" symbols)))

(defun read-string (string)
  "Read from a string until the end."
  (with-input-from-string (stream string)
    (loop
       :for form := (read stream nil nil)
       :while form
       :collect form)))

(defun prefixed-p (string prefix)
  "Whether a string starts with a prefix not counting whitespace."
  (starts-with-p (string-left-trim " " string)
		 prefix))

;; stolen from rosettacode lmao
(defun starts-with-p (str1 str2)
  "Determine whether `str1` starts with `str2`"
  (let ((p (search str2 str1)))
    (and p (= 0 p))))
