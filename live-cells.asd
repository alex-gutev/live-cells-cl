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
                 (:file "codegen")
                 (:file "observer")
                 (:file "base-cell")
                 (:file "tracker")
                 (:file "observer-cell")
                 (:file "changes-only-state")
                 (:file "computed-cell")
                 (:file "mutable-cell")
                 (:file "watch-function")
                 (:file "macros"))))

  :depends-on (:alexandria
               :anaphora
               :arrows
               :generic-cl))
