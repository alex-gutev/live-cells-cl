;;;; Defines the packages holding the test suites for live-cells

(defpackage :live-cells/test
  (:use
   :generic-cl
   :live-cells
   :fiveam)

  (:import-from
   :alexandria
   :with-gensyms)

  (:export
   :live-cells
   :test-live-cells

   ;; Test utilities
   :test-value-observer
   :observe-values
   :with-observed-values
   :test-cell-error))
