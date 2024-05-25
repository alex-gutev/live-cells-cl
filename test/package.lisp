;;;; Defines the packages holding the test suites for live-cells

(defpackage :live-cells/test
  (:use
   :generic-cl
   :arrows
   :live-cells
   :fiveam)

  (:import-from
   :alexandria
   :with-gensyms)

  (:import-from
   :live-cells
   :cell-spec
   :generate-init
   :generate-pause
   :make-local-cell-definition%)

  (:export
   :live-cells
   :test-live-cells

   ;; Test utilities
   :test-value-observer
   :observe-values
   :with-observed-values
   :test-cell-error
   :lifecycle-test-cell

   :make-lifecycle-counter
   :lifecycle-counter-init
   :lifecycle-counter-pause))
