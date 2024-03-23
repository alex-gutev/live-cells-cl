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
		 (:file "observer")
		 (:file "base-cell")
		 (:file "tracker")
		 (:file "observer-cell")
		 (:file "computed-cell")
		 (:file "watch-function"))))

  :depends-on (:alexandria
	       :anaphora
	       :arrows
	       :generic-cl))
