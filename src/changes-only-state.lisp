;;; Provides a mixin that changes the behaviour of a cell, such that
;;; it only notifies its observers if its value has actually changed.

(in-package :live-cells)

(defclass changes-only-cell-spec (observer-cell-spec)
  ((has-old-value?
    :initarg :has-old-value?
    :initform (cell-symbol 'cell 'has-old-value?)
    :accessor has-old-value?
    :documentation
    "Name of the variable holding the flag for whether the previous
    value has been recorded?")

   (old-value
    :initarg :old-value
    :initform (cell-symbol 'cell 'old-value)
    :accessor old-value
    :documentation
    "Name of the variable holding the previous value of the cell."))

  (:documentation
   "Specification for a cell that only notifies its observers when its new value is /= to its previous value.

This class can be used as mixin in existing cell classes. This means
that when this class is added as a super class of another cell class,
the behaviour of the cell it is added to is changed, such that it only
notifies its observers when its new value is not equal (by /=) to its
previous value.

NOTE: This also changes the behaviour of a cell from lazy evaluation,
when the value of the cell is referenced, to eager evaluation
whenever the values of its dependencies change."))

(defmethod generate-pre-update ((spec changes-only-cell-spec))
  (with-slots (has-old-value? old-value value) spec
    `(progn
       ,(call-next-method)

       (handler-case
           (progn
             (setf ,has-old-value? t
                   ,old-value (,value)))

         (error ()
           (setf ,has-old-value? nil
                 ,old-value nil))))))

(defmethod generate-post-update ((spec changes-only-cell-spec))
  (with-slots (has-old-value? old-value) spec
    `(progn
       ,(call-next-method)

       (setf ,has-old-value? nil
             ,old-value nil))))

(defmethod generate-did-change? ((spec changes-only-cell-spec))
  (with-slots (has-old-value? old-value value) spec
    `(handler-case (or (not ,has-old-value?)
                       (/= (,value) ,old-value))

       (error () t))))

(defmethod generate-cell-variables ((spec changes-only-cell-spec))
  (with-slots (old-value has-old-value?) spec
    (list*
     (make-variable-spec
      :name old-value
      :initform nil
      :type :variable)

     (make-variable-spec
      :name has-old-value?
      :initform nil
      :type :variable)

     (call-next-method))))
