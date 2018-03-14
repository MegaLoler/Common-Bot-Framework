(in-package :uncommon-bot.base)

(defvar *iteration-max* 1000)

(defclass basic-environment (bot-environment)
  ((syntax
    :initarg :syntax
    :initform (make-instance 'bot-syntax)
    :accessor local-syntax)
   (semantics
    :initarg :semantics
    :initform (make-instance 'bot-semantics)
    :accessor local-semantics)
   (text-protocol
    :initarg :text-protocol
    :initform (make-instance 'text-protocol)
    :accessor local-text-protocol)
   (language
    :initarg :language
    :initform (make-instance 'language)
    :accessor local-language)
   (personality
    :initarg :personality
    :initform (make-instance 'personality)
    :accessor local-personality)
   (commands
    :initarg :commands
    :initform (vector
	       (make-instance 'add-command)
	       (make-instance 'subtract-command)
	       (make-instance 'multiply-command)
	       (make-instance 'divide-command)
	       (make-instance 'quote-command)
	       (make-instance 'list-command)
	       (make-instance 'cons-command)
	       (make-instance 'eval-command)
	       (make-instance 'do-command)
	       (make-instance 'apply-command)
	       (make-instance 'invoke-command)
	       (make-instance 'command-command)
	       (make-instance 'commands-command)
	       (make-instance 'aliases-command)
	       (make-instance 'head-command)
	       (make-instance 'tail-command)
	       (make-instance 'binding-command)
	       (make-instance 'bindings-command)
	       (make-instance 'bind-command)
	       (make-instance 'binds-command)
	       (make-instance 'increment-command)
	       (make-instance 'decrement-command)
	       (make-instance 'let-command)
	       (make-instance 'lexical-lambda-command)
	       (make-instance 'dynamic-lambda-command)
	       (make-instance 'range-command)
	       (make-instance 'map-command)
	       (make-instance 'filter-command)
	       (make-instance 'fold-command)
	       (make-instance 'and-command)
	       (make-instance 'or-command)
	       (make-instance 'not-command)
	       (make-instance 'equals-command)
	       (make-instance 'greater-command)
	       (make-instance 'less-command)
	       (make-instance 'if-command)
	       (make-instance 'when-command)
	       (make-instance 'make-command)
	       (make-instance 'delete-command))
    :accessor local-commands))
  (:documentation "A basic environment loaded with some basic assets."))
