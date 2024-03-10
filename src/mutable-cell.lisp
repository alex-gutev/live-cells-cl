;;; Provides the mutable cell interface and an implementation of a
;;; basic mutable cell.

(in-package :live-cells.base)

;;; Mutable cell interface

(defvar *batch-update-p* nil
  "Is a batch update of mutable cells in effect?")

(defvar *batch-update-states* nil
  "Set of the `CELL-STATE's corresponding to the mutable cells that
   were set during the current batch.")

(define-condition inactive-mutable-cell-error (error)
  ((cell
    :reader error-cell
    :initarg :cell
    :documentation
    "The cell which caused the error"))

  (:documentation
   "Represents an attempt to read/write the value of a mutable cell
    that does not have an active state."))

(defclass mutable-cell-state (cell-state)
  ((value
    :initarg :value
    :accessor cell-state-value
    :documentation
    "The value of the mutable cell"))

  (:documentation
   "Maintains the state (value) of a simple mutable cell."))

(defclass mutable-cell (stateful-cell)
  ((mutable-state
    :initform nil
    :documentation
    "The `CELL-STATE' of this mutable cell.")))

(defgeneric mutable-cell-value (state)
  (:documentation
   "Retrieve the value held in the state of a mutable cell."))

(defgeneric (setf mutable-cell-value) (value state)
  (:documentation
   "Set the value held in the state of a mutable cell."))

(defgeneric create-mutable-state (cell old-state)
  (:documentation
   "Create the `CELL-STATE' for a mutable CELL.

OLD-STATE is the previous (disposed) state or NIL if this is the first
time a state is being created for CELL. OLD-STATE is only non-NIL if
CELL has a NIL key."))

;;; Basic mutable cell implementation

(defmethod mutable-cell-value ((state mutable-cell-state))
  (with-slots (value) state
    value))

(defmethod (setf mutable-cell-value) (new-value (state mutable-cell-state))
  (with-slots (value) state
    (cond
      ((disposed state)
       (setf value new-value))

      (t
       (notify-will-update state)
       (setf value new-value)

       (notify-or-batch state)))))

(defun notify-or-batch (state)
  "Notify the observers of a mutable cell that its value has changed
or add it to the current batch, if a batch update is in effect."

  (if *batch-update-p*
      (setf *batch-update-states*
	    (nadjoin state *batch-update-states*))

      (notify-update state)))

(defmethod create-state ((cell mutable-cell))
  (with-slots (mutable-state key) cell
    (when (or (null mutable-state) (disposed mutable-state))
      (->> (and (null key) mutable-state)
	   (create-mutable-state cell)
	   (setf mutable-state)))

    mutable-state))

(defmethod create-mutable-state ((cell mutable-cell) old-state)
  (make-instance
   'mutable-cell-state
   :cell cell
   :value (and old-state (cell-state-value old-state))))

(defmethod state ((cell mutable-cell))
  "Retrieve the state of a mutable cell.

If CELL does not have an active state and its key is non-null, an
`INACTIVE-MUTABLE-CELL-ERROR' condition is raised. Otherwise a new
state is created."

  (let ((state (call-next-method)))
    (or state
	(if (cell-key cell)
	    (error 'inactive-mutable-cell-error :cell cell)
	    (create-state cell)))))

(defmethod cell-value ((cell mutable-cell))
  (-> cell
      state
      mutable-cell-value))

(defmethod (setf cell-value) (new-value (cell mutable-cell))
  (-> cell
      state
      mutable-cell-value
      (setf new-value)))
