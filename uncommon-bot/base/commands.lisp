(in-package :uncommon-bot.base)

(defclass user-command (command)
  ((parameters
    :initarg :parameters
    :initform nil
    :type list
    :accessor parameters)
   (body
    :initarg :body
    :initform nil
    :type list
    :accessor body)
   (environment
    :initarg :environment
    :initform nil
    :type (or null bot-environment)
    :accessor environment))
  (:documentation "A user defined command."))

(defmethod command ((command user-command) (environment basic-environment))
  "Get a command designated by a user defined command in a basic environment."
  command)

(defmethod evaluate
    ((command user-command)
     (arguments list)
     (environment bot-environment))
  "Evaluate a user defined command."
  (let ((bindings
	 (mapcar
	  (lambda (parameter argument)
	    (list parameter argument))
	  (parameters command)
	  arguments))
	(env (or (environment command)
		 environment)))
    (invoke (command 'let environment)
	    (cons bindings (body command))
	    env)))

(defclass make-command (command)
  ((aliases
    :initarg :aliases
    :initform '(make mk make-command mk-command make-cmd mk-cmd new-command new-cmd define-command def-command define-cmd def-cmd)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to define a new command in the current environment."))

(defmethod evaluate
    ((command make-command)
     (arguments list)
     (environment bot-environment))
  "Define an new command."
  (let* ((aliases-arg (first arguments))
	 (aliases (if (listp aliases-arg)
		      aliases-arg
		      (list aliases-arg)))
	 (parameters (second arguments))
	 (body (cddr arguments))
	 (cmd (make-instance
	       'user-command
	       :aliases aliases
	       :parameters parameters
	       :body body
	       :environment environment)))
    (push-command cmd environment)
    cmd))

(defclass delete-command (command)
  ((aliases
    :initarg :aliases
    :initform '(delete del remove rem delete-command del-command delete-cmd del-cmd remove-command remove-cmd undefine-command undef-command undefine-cmd undef-cmd)
    :accessor aliases))
  (:documentation "A command to delete a command."))

(defmethod evaluate
    ((command delete-command)
     (arguments list)
     (environment bot-environment))
  "Delete a command."
  (remove-command (command (car arguments) environment)
		  environment))

(defclass lexical-lambda-command (command)
  ((aliases
    :initarg :aliases
    :initform '(lambda lexical-lambda lexical lexic lex anonymous-command anon-command anonymous-cmd anon-cmd)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to make an anonymous command with lexical binding."))

(defmethod evaluate
    ((command lexical-lambda-command)
     (arguments list)
     (environment bot-environment))
  "Make an anonymous command."
  (let ((parameters (car arguments))
	(body (cdr arguments)))
    (make-instance 'user-command
		   :parameters parameters
		   :body body
		   :environment environment)))

(defclass dynamic-lambda-command (command)
  ((aliases
    :initarg :aliases
    :initform '(dynamic-lambda dynamic dynam dyn)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to make an anonymous command with dynamic binding."))

(defmethod evaluate
    ((command dynamic-lambda-command)
     (arguments list)
     (environment bot-environment))
  "Make an anonymous command with dynamic binding."
  (let ((parameters (car arguments))
	(body (cdr arguments)))
    (make-instance 'user-command
		   :parameters parameters
		   :body body)))

(defclass add-command (command)
  ((aliases
    :initarg :aliases
    :initform '(add sum +)
    :accessor aliases))
  (:documentation "A command to add values."))

(defmethod evaluate
    ((command add-command)
     (arguments list)
     (environment bot-environment))
  "Add values."
  (reduce #'+ arguments))

(defclass subtract-command (command)
  ((aliases
    :initarg :aliases
    :initform '(subtract difference -)
    :accessor aliases))
  (:documentation "A command to subtract values."))

(defmethod evaluate
    ((command subtract-command)
     (arguments list)
     (environment bot-environment))
  "Subtract values."
  (reduce #'- arguments))

(defclass multiply-command (command)
  ((aliases
    :initarg :aliases
    :initform '(multiply product *)
    :accessor aliases))
  (:documentation "A command to multiply values."))

(defmethod evaluate
    ((command multiply-command)
     (arguments list)
     (environment bot-environment))
  "Multiply values."
  (reduce #'* arguments))

(defclass divide-command (command)
  ((aliases
    :initarg :aliases
    :initform '(divide quotient / ÷)
    :accessor aliases))
  (:documentation "A command to divide values."))

(defmethod evaluate
    ((command divide-command)
     (arguments list)
     (environment bot-environment))
  "Divide values."
  (reduce #'/ arguments))

(defclass list-command (command)
  ((aliases
    :initarg :aliases
    :initform '(list ls l)
    :accessor aliases))
  (:documentation "A command to make a list of values."))

(defmethod evaluate
    ((command list-command)
     (arguments list)
     (environment bot-environment))
  "Make a list."
  arguments)

(defclass cons-command (command)
  ((aliases
    :initarg :aliases
    :initform '(cons)
    :accessor aliases))
  (:documentation "A command to make a cons."))

(defmethod evaluate
    ((command cons-command)
     (arguments list)
     (environment bot-environment))
  "Cons two values."
  (cons (car arguments)
	(cadr arguments)))

(defclass quote-command (command)
  ((aliases
    :initarg :aliases
    :initform '(quote qu q)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to quote a value."))

(defmethod evaluate
    ((command quote-command)
     (arguments list)
     (environment bot-environment))
  "Quote a value."
  (car arguments))

(defclass eval-command (command)
  ((aliases
    :initarg :aliases
    :initform '(evaluate eval execute exe)
    :accessor aliases))
  (:documentation "A command to evaluate expressions."))

(defmethod evaluate
    ((command eval-command)
     (arguments list)
     (environment bot-environment))
  "Evaluate expressions."
  (loop
     :for expression :in arguments
     :for value = (uncommon-lisp-eval
		   nil
		   expression
		   environment)
     :finally (return value)))

(defclass do-command (command)
  ((aliases
    :initarg :aliases
    :initform '(do block progn)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to evaluate expressions without evaluating the arguments first."))

(defmethod evaluate
    ((command do-command)
     (arguments list)
     (environment bot-environment))
  "Evaluate expressions."
  (invoke (command 'eval environment)
	  arguments
	  environment))

(defclass apply-command (command)
  ((aliases
    :initarg :aliases
    :initform '(apply)
    :accessor aliases))
  (:documentation "A command to invoke a command with a list of arguments."))

(defmethod evaluate
    ((command apply-command)
     (arguments list)
     (environment bot-environment))
  "Invoke a command with list of arguments."
  (invoke (command (car arguments) environment)
	  (cadr arguments)
	  environment))

(defclass invoke-command (command)
  ((aliases
    :initarg :aliases
    :initform '(invoke do-cmd)
    :accessor aliases))
  (:documentation "A command to invoke a command with arguments."))

(defmethod evaluate
    ((command invoke-command)
     (arguments list)
     (environment bot-environment))
  "Invoke a command with list of arguments."
  (invoke (command (car arguments) environment)
	  (cdr arguments)
	  environment))

(defclass command-command (command)
  ((aliases
    :initarg :aliases
    :initform '(command cmd get-command get-cmd)
    :accessor aliases))
  (:documentation "A command to return a command by a command designator."))

(defmethod evaluate
    ((command command-command)
     (arguments list)
     (environment bot-environment))
  "Return a command."
  (command (car arguments) environment))

(defclass commands-command (command)
  ((aliases
    :initarg :aliases
    :initform '(commands cmds get-commands get-cmds all-commands all-cmds list-commands list-cmds ls-commands ls-cmds)
    :accessor aliases))
  (:documentation "A command to a list of return all commands available in the environment or commands by command designators."))

(defmethod evaluate
    ((command commands-command)
     (arguments list)
     (environment bot-environment))
  "Return commands."
  (if arguments
      (mapcar (lambda (designator)
		(command designator environment))
	      arguments)
      (commands environment)))

(defclass aliases-command (command)
  ((aliases
    :initarg :aliases
    :initform '(aliases names command-aliases command-names cmd-aliases cmd-names)
    :accessor aliases))
  (:documentation "A command to return the aliases of a command by a designator."))

(defmethod evaluate
    ((command aliases-command)
     (arguments list)
     (environment bot-environment))
  "Return aliases of a command."
  (aliases (command (car arguments) environment)))

(defclass head-command (command)
  ((aliases
    :initarg :aliases
    :initform '(head first car)
    :accessor aliases))
  (:documentation "A command to get the first item of a list."))

(defmethod evaluate
    ((command head-command)
     (arguments list)
     (environment bot-environment))
  "Return the head of a list."
  (car (car arguments)))

(defclass tail-command (command)
  ((aliases
    :initarg :aliases
    :initform '(tail rest cdr)
    :accessor aliases))
  (:documentation "A command to get the rest of a list."))

(defmethod evaluate
    ((command tail-command)
     (arguments list)
     (environment bot-environment))
  "Return the tail of a list."
  (cdr (car arguments)))

(defclass binding-command (command)
  ((aliases
    :initarg :aliases
    :initform '(get value variable var binding value-of resolve)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to get the value of a binding in the local environment."))

(defmethod evaluate
    ((command binding-command)
     (arguments list)
     (environment bot-environment))
  "Return the value of a binding."
  (let ((argument (car arguments)))
    (if (listp argument)
	(invoke (command 'bindings environment)
		argument
		environment)
	(binding argument environment))))

(defclass bindings-command (command)
  ((aliases
    :initarg :aliases
    :initform '(gets values variables vars bindings values-of resolves)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to get the values of bindings in the local environment."))

(defmethod evaluate
    ((command bindings-command)
     (arguments list)
     (environment bot-environment))
  "Return the value of some bindings."
  (mapcar (lambda (name)
	    (binding name environment))
	  arguments))

(defclass bind-command (command)
  ((aliases
    :initarg :aliases
    :initform '(set set-variable set-var bind set-value-of define def declare)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to set the value of a binding in the local environment."))

(defmethod evaluate
    ((command bind-command)
     (arguments list)
     (environment bot-environment))
  "Bind a name (or names) to a value and return the value."
  (let* ((argument (car arguments))
	 (args (if (listp argument)
		   argument
		   (list argument)))
	 (value (cadr arguments)))
    (mapcar (lambda (name)
	      (bind name value environment))
	    args)
    value))

(defclass binds-command (command)
  ((aliases
    :initarg :aliases
    :initform '(sets set-variables set-vars binds set-values-of defines defs declares)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to set the values of multiple bindings in the local environment."))

(defmethod evaluate
    ((command binds-command)
     (arguments list)
     (environment bot-environment))
  "Bind names to values and return the values."
  (let* ((argument (car arguments))
	 (args (if (listp argument)
		   argument
		   (list argument)))
	 (values (cdr arguments)))
    (mapcar (lambda (name value)
	      (bind name value environment))
	    args values)
    values))

(defclass let-command (command)
  ((aliases
    :initarg :aliases
    :initform '(let)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to evaluate expressions with bindings."))

(defmethod evaluate
    ((command let-command)
     (arguments list)
     (environment bot-environment))
  "Evaluate expressions with bindings."
  (let ((bindings (car arguments))
	(expressions (cdr arguments))
	(env (make-instance 'bot-environment
			    :parent environment)))
    (loop
       :for binding :in bindings
       :for name = (car binding)
       :for value = (uncommon-lisp-eval
		     nil
		     (cadr binding)
		     environment)
       :do (bind name value env t))
    (invoke (command 'do environment)
	    expressions
	    env)))

;; these two commands (inc and dec) should be generalized into Modify or something
(defclass increment-command (command)
  ((aliases
    :initarg :aliases
    :initform '(increment inc ++)
    :accessor aliases))
  (:documentation "A command to increment the values associated with bindings."))

(defmethod evaluate
    ((command increment-command)
     (arguments list)
     (environment bot-environment))
  "Increment bound values."
  (let ((result
	 (uncommon-lisp-eval
	  nil
	  `(sets ',arguments
		 ,@(loop
		      :for argument :in arguments
		      :collect `(+ (get ,argument) 1)))
	  environment)))
    (if (cdr arguments)
	result
	(car result))))

(defclass decrement-command (command)
  ((aliases
    :initarg :aliases
    :initform '(decrement dec --)
    :accessor aliases))
  (:documentation "A command to decrement the values associated with bindings."))

(defmethod evaluate
    ((command decrement-command)
     (arguments list)
     (environment bot-environment))
  "Decrement bound values."
  (let ((result
	 (uncommon-lisp-eval
	  nil
	  `(sets ',arguments
		 ,@(loop
		      :for argument :in arguments
		      :collect `(- (get ,argument) 1)))
	  environment)))
    (if (cdr arguments)
	result
	(car result))))

(defclass range-command (command)
  ((aliases
    :initarg :aliases
    :initform '(range)
    :accessor aliases))
  (:documentation "A command to get a list of integers in a range."))

(defmethod evaluate
    ((command range-command)
     (arguments list)
     (environment bot-environment))
  "List integers in a range."
  (let ((min (if (= (length arguments) 1)
		 1
		 (car arguments)))
	(max (if (= (length arguments) 1)
		 (car arguments)
		 (cadr arguments)))
	(step (if (< (length arguments) 3)
		  1
		  (caddr arguments))))
    (if (> (abs (- max min))
	   *iteration-max*)
	(error "That range is too large!")
	(if (< min max)
	    (loop
	       :for n
	       :from min
	       :to max
	       :by step
	       :collect n)
	    (loop
	       :for n
	       :from min
	       :downto max
	       :by step
	       :collect n)))))

(defclass map-command (command)
  ((aliases
    :initarg :aliases
    :initform '(map map-command map-cmd)
    :accessor aliases))
  (:documentation "A command to map a command to some lists."))

(defmethod evaluate
    ((command map-command)
     (arguments list)
     (environment bot-environment))
  "Map a command to lists."
  (let* ((lists (cdr arguments))
	 (cmd (car arguments))
	 (arglists (apply #'mapcar
		      (cons #'list
			    lists))))
    (loop
       :for arglist :in arglists
       :for value = (invoke (command cmd environment)
			    arglist
			    environment)
       :collect value)))

(defclass filter-command (command)
  ((aliases
    :initarg :aliases
    :initform '(filter filter-command filter-cmd)
    :accessor aliases))
  (:documentation "A command to filter values in a list."))

(defmethod evaluate
    ((command filter-command)
     (arguments list)
     (environment bot-environment))
  "Filter values."
  (let* ((list (cadr arguments))
	 (cmd (car arguments)))
    (remove-if-not (lambda (value)
		     (truep
		      (invoke (command cmd environment)
			      (list value)
			      environment)))
		   list)))

(defclass fold-command (command)
  ((aliases
    :initarg :aliases
    :initform '(fold fold-command fold-cmd reduce reduce-command reduce-cmd)
    :accessor aliases))
  (:documentation "A command to fold values in a list."))

(defmethod evaluate
    ((command fold-command)
     (arguments list)
     (environment bot-environment))
  "Fold values."
  (let* ((list (cadr arguments))
	 (cmd (car arguments)))
    (reduce (lambda (a b)
	      (invoke (command cmd environment)
		      (list a b)
		      environment))
	    list)))

(defclass not-command (command)
  ((aliases
    :initarg :aliases
    :initform '(not negate ~ !)
    :accessor aliases))
  (:documentation "A command to get the logical negation of a value."))

(defmethod evaluate
    ((command not-command)
     (arguments list)
     (environment bot-environment))
  "Negate values."
  (truth-wrap (not (truep (car arguments)))))

(defclass and-command (command)
  ((aliases
    :initarg :aliases
    :initform '(and all & &&)
    :accessor aliases))
  (:documentation "A command to get the logical and of some values."))

(defmethod evaluate
    ((command and-command)
     (arguments list)
     (environment bot-environment))
  "And values."
  (truth-wrap
   (reduce (lambda (a b)
	     (and (truep a)
		  (truep b)))
	   arguments)))

(defclass or-command (command)
  ((aliases
    :initarg :aliases
    :initform '(or any)
    :accessor aliases))
  (:documentation "A command to get the logical or of some values."))

(defmethod evaluate
    ((command or-command)
     (arguments list)
     (environment bot-environment))
  "Or values."
  (truth-wrap
   (reduce (lambda (a b)
	     (or (truep a)
		 (truep b)))
	   arguments)))

(defclass equals-command (command)
  ((aliases
    :initarg :aliases
    :initform '(= == eq equal eql equals is same identical match matches)
    :accessor aliases))
  (:documentation "A command to get whether values are equal."))

(defmethod evaluate
    ((command equals-command)
     (arguments list)
     (environment bot-environment))
  "Test equality."
  (truth-wrap
   (reduce (lambda (a b)
	     (equalp a b))
	   arguments)))

(defclass greater-command (command)
  ((aliases
    :initarg :aliases
    :initform '(greater greater-than more more-than bigger bigger-than larger larger-than higher higher-than above over >)
    :accessor aliases))
  (:documentation "A command to get whether a value is greater than another."))

(defmethod evaluate
    ((command greater-command)
     (arguments list)
     (environment bot-environment))
  "Test for greater than."
  (truth-wrap (> (first arguments)
		 (second arguments))))

(defclass less-command (command)
  ((aliases
    :initarg :aliases
    :initform '(less less-than smaller smaller-than littler littler-than lower lower-than below under <)
    :accessor aliases))
  (:documentation "A command to get whether a value is less than another."))

(defmethod evaluate
    ((command less-command)
     (arguments list)
     (environment bot-environment))
  "Test for less than."
  (truth-wrap (< (first arguments)
		 (second arguments))))

(defclass if-command (command)
  ((aliases
    :initarg :aliases
    :initform '(if)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to conditionally evaluate an expression."))

(defmethod evaluate
    ((command if-command)
     (arguments list)
     (environment bot-environment))
  "Conditionally evaluate."
  (let ((predicate (car arguments))
	(true-expr (cadr arguments))
	(false-expr (caddr arguments)))
    (if (truep (uncommon-lisp-eval nil predicate environment))
	(uncommon-lisp-eval nil true-expr environment)
	(uncommon-lisp-eval nil false-expr environment))))

(defclass when-command (command)
  ((aliases
    :initarg :aliases
    :initform '(when)
    :accessor aliases)
   (eval-args
    :initarg :eval-args
    :initform nil
    :accessor eval-args))
  (:documentation "A command to conditionally evaluate an expression."))

(defmethod evaluate
    ((command when-command)
     (arguments list)
     (environment bot-environment))
  "Conditionally evaluate."
  (let ((predicate (car arguments))
	(body (cdr arguments)))
    (if (truep (uncommon-lisp-eval nil predicate environment))
	(invoke (command 'do environment)
		body
		environment))))

(defclass print-command (command)
  ((aliases
    :initarg :aliases
    :initform '(say print echo put write)
    :accessor aliases))
  (:documentation "A command to print out values."))

(defmethod evaluate
    ((command print-command)
     (arguments list)
     (environment bot-environment))
  "Print values."
  (uncommon-lisp-print nil arguments *standard-output* environment)
  (format t "~%"))
