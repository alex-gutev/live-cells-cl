;;; Provides the OBSERVER-CELL-STATE which defines functionality for
;;; observing other cells in a `CELL-STATE'.

(in-package :live-cells.base)

;;; State for cells observing other cells

(defclass observer-cell-state (cell-state)
  ((stale?
    :initform t
    :accessor stale?
    :documentation
    "Should the value of this cell be recomputed?")

   (updating?
    :initform nil
    :accessor updating?
    :documentation
    "Is a cell value update currently in progress?")

   (waiting-for-change?
    :initform nil
    :accessor waiting-for-change?
    :documentation
    "Is this state waiting for an argument cell to change is value?"))

  (:documentation
   "A `CELL-STATE' which observes other cells and triggers updates whenever their values change.

This state implements the `CELL-OBSERVER' methods"))

(defgeneric did-change? (state)
  (:documentation
   "Has the value of the cell changed during the current update cycle?

This function is called before calling UPDATE on this cell's
observers. Methods should return NIL if it is known for sure that the
cell's value has not changed in the current update cycle, otherwise
true should be returned.")

  (:method ((state observer-cell-state))
    "Return true which indicates the value may have changed."

    t))

(defgeneric pre-update (state)
  (:documentation
   "Run actions prior to notifying the observers that the cell's value will change.

This method is called before calling WILL-UPDATE on this cell's
observers.")

  (:method ((state observer-cell-state))
    nil))

(defgeneric post-update (state)
  (:documentation
   "Run actions after the cell's value has changed.")

  (:method ((state observer-cell-state))
    nil))

(defgeneric on-will-update (state)
  (:documentation
   "Notify the observers that the value of this cell will change.

This allows subclasses to provide a method that prevents the observers
from being notified.")

  (:method ((state observer-cell-state))
    (notify-will-update state)))

(defgeneric on-update (state did-change?)
  (:documentation
   "Notify the observers that the value of this cell has changed.

This allows subclasses to provide a method that prevents the observers
from being notified.")

  (:method ((state observer-cell-state) did-change?)
    (notify-update state)))

;;; CELL-OBSERVER methods

(defmethod will-update ((state observer-cell-state) cell)
  (with-slots (updating? waiting-for-change? stale?) state
    (when (not updating?)
      (pre-update state)

      (setf updating? t
	    waiting-for-change? nil)

      (on-will-update state)
      (setf stale? t))))

(defmethod update ((state observer-cell-state) cell)
  (with-slots (updating? waiting-for-change? stale?) state
    (when (or updating? waiting-for-change?)
      (on-update state (did-change? state))

      (setf waiting-for-change? nil
	    updating? nil)

      (post-update state))))
