;;;; Implementation of (NONE) for stopping a computation.

(in-package :live-cells)

(define-condition stop-computation (condition)
  ((default-value
    :reader default-value
    :initarg :default-value
    :initform nil))

  (:documentation
   "Condition signaling to stop the computation of a cell's value"))

(defun none (&optional default-value)
  "Stop the computation of a cell's value.

When this function is called within the value form of cell, the
computation of the cell's value is stopped, the value form is exited
and the cell's current value is preserved.

If this function is called while computing the initial value of a
cell, the value of the cell is set to DEFAULT-VALUE.

The symbol-macro NONE is a synonym for ``(NONE)``.

This function works by signaling a STOP-COMPUTATION condition, which
is then handled by the cell. It is important that this condition is
not handled by the value form of the cell, otherwise this function
will have no effect."

  (error 'stop-computation :default-value default-value))

;; Synonym for (NONE)

(define-symbol-macro none (none))
