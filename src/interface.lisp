;;;; Provides the base cell and observer interfaces.

(in-package :live-cells.base)

;;; Cell Interface

(defgeneric cell-value (cell)
  (:documentation
   "Retrieve the value of a CELL.

This method should be implemented by all cells."))

(defgeneric (setf cell-value) (value cell)
  (:documentation
   "Set the value of a CELL.

This method should only be implemented by mutable cells."))

(defgeneric add-observer (cell observer)
  (:documentation
   "Add an OBSERVER, that is notified when the value of CELL changes.

This method should be implemented by all cells."))

(defgeneric remove-observer (cell observer)
  (:documentation
   "Remove an OBSERVER from the observers of CELL.

NOTE: If ADD-OBSERVER was called N times with the same CELL and
OBSERVER, this function must also be called N times before OBSERVER is
actually removed from the observers of CELL.

Once OBSERVER is removed, it will no longer be called when the value
of CELL changes."))


;;; Observer Interface

(defgeneric will-update (observer cell)
  (:documentation
   "Notify OBSERVER that the value of CELL will change.

This function is called before the value of CELL changes. (CELL-VALUE
CELL) still returns the cell's previous value"))

(defgeneric update (observer cell)
  (:documentation "Notify OBSERVER that the value of CELL has changed.

This function is called after the value of CELL has
changed. (CELL-VALUE CELL) now returns the new value."))


;;; Constant cell implementations

(defmethod cell-value (value)
  "Returns VALUE.

This method allows ordinary values to function as cells."

  value)

(defmethod add-observer (cell observer)
  "Does nothing.

The purpose of this method is to allow ordinary values to function as
cells."

  nil)

(defmethod remove-observer (cell observer)
  "Does nothing.

The purpose of this method is to allow ordinary values to function as
cells."

  nil)
