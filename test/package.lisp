;;;; Defines the packages holding the test suites for live-cells

(defpackage :live-cells/test
  (:use
   :generic-cl
   :live-cells
   :fiveam)

  (:import-from
   :alexandria
   :with-gensyms))
