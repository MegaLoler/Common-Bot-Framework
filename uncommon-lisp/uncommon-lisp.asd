(defsystem #:uncommon-lisp
    :description "A simple object-oriented framework for building languages."
    :author "MegaLoler"
    :serial t
    :components ((:module "core"
			  :components
                          ((:file "package")
			   (:file "environment")
                           (:file "uncommon-lisp")
                           (:file "syntax")
                           (:file "semantics")
                           (:file "repl")))))
