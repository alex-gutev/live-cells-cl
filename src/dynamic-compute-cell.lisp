;;; Provides the definition of a computed cell that determines its
;;; argument cells dynamically.

(in-package :live-cells.base)

(defclass dynamic-compute-cell (stateful-cell)
  ((compute
    :initarg :compute
    :accessor compute
    :documentation
    "Function that computes the value of the cell.

NOTE: The argument cells of the function should be references using
USE-VALUE."))

  (:documentation
   "A computed cell which determines its arguments dynamically."))

(defmethod cell-value ((cell dynamic-compute-cell))
  (with-slots (state compute) cell
    (if state
	(cell-value state)
	(without-tracker
	  (funcall compute)))))

;;; `CELL-STATE' definition

(defmethod create-state ((cell dynamic-compute-cell))
  (make-instance 'dynamic-compute-cell-state :cell cell))

(defclass dynamic-compute-cell-state (compute-cell-state)
  ())

(defmethod compute ((state dynamic-compute-cell-state))
  (with-slots (cell arguments) state
    (with-tracker ((arg)
		    (when (not (memberp arg arguments))
		      (add-observer arg state)
		      (->> (nadjoin arg arguments)
			   (setf arguments))))
      (funcall (compute cell)))))

(defmethod init ((state dynamic-compute-cell-state))
  (call-next-method)

  (with-slots (value) state
    (setf value (cell-value state))))
