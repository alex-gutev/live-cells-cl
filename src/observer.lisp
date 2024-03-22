;;;; Provides the cell observer and argument record structure
;;;; definitions.

(in-package :live-cells)


;;; Cell Observer Record

(defstruct observer
  "Identifies a cell observer.

KEY is the cell identifier.

WILL-UPDATE and UPDATE are the 'will update' and 'update' functions,
respectively"

  key
  will-update
  update)

(defmethod equalp ((a observer) (b observer))
  (= (observer-key a) (observer-key b)))

(defmethod hash ((o observer))
  (hash (observer-key o)))

(defun call-will-update (observer arg)
  "Call the 'will update' OBSERVER method.

ARG is the `ARGUMENT' identifying the argument cell."

  (funcall (observer-will-update observer) arg))

(defun call-update (observer arg)
  "Call the 'update' OBSERVER method.

ARG is the `ARGUMENT' identifying the argument cell."

  (funcall (observer-update observer) arg))


;;; Cell Argument record

(defstruct argument
  "Identifies a cell argument.

KEY is the argument cell identifier.

ADD-OBSERVER and REMOVE-OBSERVER are the functions for adding and
removing observers respectively."

  key
  add-observer
  remove-observer)

(defmethod equalp ((a argument) (b argument))
  (= (argument-key a) (argument-key b)))

(defmethod hash ((a argument))
  (hash (argument-key a)))

(defun call-add-observer (arg observer)
  "Add an OBSERVER to an `ARGUMENT' cell ARG."

  (funcall (argument-add-observer arg) observer))

(defun call-remove-observer (arg observer)
  "Remove an OBSERVER from an `ARGUMENT' cell ARG."

  (funcall (argument-remove-observer arg) observer))
