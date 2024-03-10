;;;; Provides the definitions of the base classes providing the
;;;; stateful cell functionality.

(in-package :live-cells.base)

;;; Cell State

(defvar *cell-state-table* (make-hash-map)
  "Global table mapping cell keys to the corresponding cell state.")

(defclass cell-state ()
  ((key
    :initarg :key
    :initform nil
    :accessor state-key

    :documentation
    "Key associating this state to its cell.")

   (cell
    :initarg :cell
    :accessor state-cell

    :documentation
    "The cell to which this state is associated.")

   (observers
    :initform (make-hash-map)
    :accessor observers

    :documentation
    "Map storing the observers of this cell.

The keys are the actual observers whereas the values, are the number
of times ADD-OBSERVER has been called for a particular observer.")

   (diposed?
    :initform nil
    :accessor disposed?

    :documentation
    "Has this state been disposed?

If true this state should no longer be used, and a new state should be
created."))

  (:documentation
   "Base class for defining the state of a `STATEFUL-CELL'"))

(defgeneric init (state)
  (:documentation
   "Initialize a `CELL-STATE'.

This functions is called before the first observer is added on the
cell. A method should be implemented that contains the setup logic,
such as adding observers to other cells, which is necessary in order
for this cell to function."))

(defgeneric dispose (state)
  (:documentation
   "Dispose a `CELL-STATE'.

This function is called after the last observer is removed from the
cell. A method should be implemented that contains the teardown logic,
such as removing observers that were added in INIT, of the cell.

NOTE: All methods must call CALL-NEXT-METHOD."))

(defgeneric notify-update (state)
  (:documentation
   "Notify the observers that the cell's value has changed."))

(defgeneric notify-will-update (state)
  (:documentation
   "Notify the observers that the cell's value will change."))


;;; Stateful Cell

(defclass stateful-cell ()
  ((key
    :initarg :key
    :initform nil
    :accessor cell-key

    :documentation
    "Key that identifies the cell and associates it to its state.

If this is NIL then it means this cell references a unique state that
is not referenced by any other `STATEFUL-CELL' object.")

   (state
    :initform nil
    :documentation
    "The `CELL-STATE' maintaining the state of this cell.")))

(defgeneric create-state (cell)
  (:documentation
   "Create the `CELL-STATE' for a `STATEFUL-CELL'

This function must be implemented by all `STATEFUL-CELL'
subclasses."))

(defgeneric state (cell)
  (:documentation
   "Retrieve the `CELL-STATE' of a `STATEFUL-CELL'

If the state has been disposed or none has been initialized, NIL is
returned."))


;;; Cell State Implementation

(defmethod init ((state cell-state))
  nil)

(defmethod dispose ((state cell-state))
  (erase *cell-state-table* (state-key state))
  (setf (disposed state) t))

(defmethod add-observer ((state cell-state) observer)
  (with-accessors ((observers observers)) state
    (when (emptyp observers)
      (init state))

    (setf (get observer observers)
	  (1+ (ensure-get observer observers 0)))))

(defmethod remove-observer ((state cell-state) observer)
  (with-accessors ((observers observers)) state
    (awhen (get observer observers)
      (cond
	((> it 1)
	 (decf (get observer observers)))

	(t
	 (erase observers observer)

	 (when (emptyp observers)
	   (dispose state)))))))

(defmethod notify-will-update ((state cell-state))
  (with-accessors ((observers observers)
		   (cell state-cell))
      state

    (doseq (observer (map-keys observers))
      (will-update observer cell))))

(defmethod notify-update ((state cell-state))
  (with-accessors ((observers observers)
		   (cell state-cell))
      state

    (doseq (observer (map-keys observers))
      (update observer cell))))


;;; Stateful Cell Implementation

(defmacro get-cell-state (key &body make-state)
  "Retrieve the `CELL-STATE' corresponding to the cell identified by KEY.

If the state does not exist, the forms in MAKE-STATE are evaluated
with the state initialized to the value returned by the last form in
MAKE-STATE."

  `(if key
       (ensure-get ,key *cell-state-table* ,@make-state)
       (progn
	 ,@make-state)))

(defun ensure-cell-state (cell)
  "Retrieve the state of a `STATEFUL-CELL', creating it if it doesn't exist."

  (with-slots (state key) cell
    (when (or (null state) (disposed state))
      (setf state (get-cell-state key (create-state cell))))

    state))

(defmethod state ((cell stateful-cell))
  "Retrieve the `CELL-STATE' of a `STATEFUL-CELL'

If the state has been disposed or none has been initialized, NIL is
returned."

  (with-slots (key state) cell
    (when (or (null state) (disposed state))
      (setf state (get key *cell-state-table*)))

    state))

(defmethod add-observer ((cell stateful-cell) observer)
  "Add an observer to the observer set of the `STATEFUL-CELL'."

  (add-observer (ensure-cell-state cell) observer))

(defmethod remove-observer ((cell stateful-cell) observer)
  "Remove an observer from the observer set of the `STATEFUL-CELL'."

  (remove-observer (state cell) observer))
