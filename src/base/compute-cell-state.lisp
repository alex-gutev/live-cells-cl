;;; Provides the state class for computed cells.

(in-package :live-cells.base)

(defclass compute-cell-state (observer-cell-state)
  ((arguments
    :initarg :arguments
    :initform (make-hash-set)
    :accessor arguments

    :documentation
    "Set of argument cells on which the value of this cell depends.")

   (value
    :initform nil
    :documentation
    "The value of the cell.")

   (has-value?
    :initform nil
    :documentation
    "Has a value been computed?"))

  (:documentation
   "A `CELL-STATE' for a cell of which the value is a
   function (defined by COMPUTE) of one or more argument cells."))

(defgeneric compute (state)
  (:documentation
   "Compute the value of the cell associated with the `COMPUTE-CELL-STATE' STATE.

This method is called when the value of the cell should be
computed. NOTE: This is not necessarily called every time the argument
cells change, since the cell might not be referenced.

Subclasses of `COMPUTE-CELL-STATE' should provide a method that
computes and returns the value of the cell."))

;;; CELL-STATE methods

(defmethod init ((state compute-cell-state))
  "Adds STATE as an observer of the argument cells."

  (call-next-method)

  (doseq (arg (arguments state))
    (add-observer arg state)))

(defmethod dispose ((state compute-cell-state))
  "Remove STATE from the observers of the argument cells."

  (doseq (arg (arguments state))
    (remove-observer arg state))

  (call-next-method))

(defmethod cell-value ((state compute-cell-state))
  "Compute the value of the cell.

If the argument cells have not changed, since the last time the value
was computed, the previously computed value is returned."

  (with-slots (stale? has-value? value) state
    (when stale?
      (setf value (compute state)
	    stale? nil
	    has-value? t))

    value))
