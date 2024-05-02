;;;; Implementation of (NONE) for stopping a computation.

(in-package :live-cells)

(define-condition stop-computation (condition)
  ()

  (:documentation
   "Condition signaling to stop the computation of a cell's value"))

(defun none ()
  "Stop the computation of a cell's value

When this function is called within a computed cell, the computation
of the cell's value is stopped and its current value is preserved."

  (error 'stop-computation))

;; Synonym for (NONE)

(define-symbol-macro none (none))
