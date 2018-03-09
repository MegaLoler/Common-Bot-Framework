(in-package :common-bot)

(deftype lambda-list-keyword ()
  "Whether something is a lambda list keyword."
  `(member &optional &key &rest &aux &whole))

;; copied from rosettacode lmao
(defun starts-with-p (str1 str2)
  "Determine whether `str1` starts with `str2`"
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

(defun starts-with-any (string strings)
  "Whether `string' starts with any of `strings'."
  (reduce (lambda (acc val)
	    (or acc (starts-with-p string val)))
	  strings))
