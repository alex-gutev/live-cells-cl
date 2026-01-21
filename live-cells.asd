(asdf:defsystem #:live-cells
  :description "Reactive programming for lisp"
  :author "Alexander Gutev"
  :license "MIT"
  :version "0.2.0"
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
                 (:file "stop-compute")
                 (:file "computed-cell")
                 (:file "mutable-cell")
                 (:file "watch-function")
                 (:file "peek")
                 (:file "macros"))))

  :depends-on (:alexandria
               :anaphora
               :arrows
               :generic-cl)

  :in-order-to ((asdf:test-op (asdf:test-op :live-cells/test))))

(asdf:defsystem #:live-cells/test
  :description "Tests suites for live-cells"
  :author "Alexander Gutev"
  :license "MIT"
  :depends-on (#:live-cells #:fiveam)
  :serial t
  :components ((:module
                "test"
                :serial t
                :components
                ((:file "package")
                 (:file "test")
                 (:file "mutable-cell")
                 (:file "computed-cell")
                 (:file "consistency")
                 (:file "init-cleanup")
                 (:file "watch")
                 (:file "peek"))))

  :perform (asdf:test-op :after (op c)
                         (uiop:symbol-call :live-cells/test :test-live-cells)))
