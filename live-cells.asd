(asdf:defsystem #:live-cells
  :description "Reactive programming for lisp"
  :author "Alexander Gutev"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :components ((:module
		"src"
		:serial t
		:components
		((:file "package")
		 (:file "interface")
		 (:file "compute-cell")
		 (:file "stateful-cell")
		 (:file "mutable-cell")
		 (:file "watch"))))

  :depends-on (:alexandria
	       :anaphora
	       :arrows
	       :generic-cl))
