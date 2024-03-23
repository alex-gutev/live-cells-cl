;;; Provides functionality for generating the definition of a cell, of
;;; which the value should be recomputed whenever the arguments of the
;;; cell change.

(in-package :live-cells)


;;; Interface

(defclass observer-cell-spec (cell-spec)
  ((stale?
    :initarg :stale?
    :initform (cell-symbol 'cell 'stale?)
    :accessor stale?
    :documentation
    "Name of the variable holding the flag for whether the cell's
    value should be recomputed.")

   (updating?
    :initarg :updating?
    :initform (cell-symbol 'cell 'updating?)
    :accessor updating?
    :documentation
    "Name of the variable holding the flag for whether the cell's
    value is being recomputed.")

   (will-update
    :initarg :will-update
    :initform (cell-symbol 'cell 'will-update)
    :accessor will-update
    :documentation
    "Name of the variable holding the cells 'will-update' function.")

   (update
    :initarg :update
    :initform (cell-symbol 'cell 'update)
    :accessor update
    :documentation
    "Name of the variable holding the cells 'update' function."))

  (:documentation
   "Specification for a cell of which the value is recomputed when the
   values of its argument cells change."))

(defgeneric generate-pre-update (spec)
  (:documentation
   "Generate a form which is run before the update cycle for the cell defined by SPEC begins.

The generated form is evaluated when the 'will-update' function is
called for the first time of an update cycle for the cell."))

(defgeneric generate-post-update (spec)
  (:documentation
   "Generate a form which is run after the value of the cell defined by SPEC is changed.

The generated form is inserted in the 'update' function of the cell,
after the value is updated."))

(defgeneric generate-on-will-update (spec)
  (:documentation
   "Generate the form which calls the 'will-update' function of the
   observers of the cell defined by SPEC."))

(defgeneric generate-on-update (spec)
  (:documentation
   "Generate the form which calls the 'update' function of the
   observers of the cell defined by SPEC."))

(defgeneric generate-will-update (spec arg)
  (:documentation
   "Generate the 'will-update' function of the cell defined by SPEC.

The return value should be a single form implementing the function,
with ARG being the symbol to use for the function argument."))

(defgeneric generate-update (spec arg)
  (:documentation
   "Generate the 'update' function of the cell defined by SPEC.

The return value should be a single form implementing the function,
with ARG being the symbol to use for the function argument."))


;;; Implementations

(defmethod generate-will-update ((spec observer-cell-spec) arg)
  (with-slots (updating? stale?) spec
    `(progn
       (when (not ,updating?)
	 ,@(awhen (generate-pre-update spec)
	     `(,it))

	 (setf ,updating? t)

	 ,@(awhen (generate-on-will-update spec)
	     `(,it))

	 (setf ,stale? t)))))

(defmethod generate-update ((spec observer-cell-spec) arg)
  (with-slots (updating? stale?) spec
    `(progn
       (when ,updating?
	 ,@(awhen (generate-on-update spec)
	     `(,it))

	 (setf ,updating? nil)

	 ,@(awhen (generate-post-update spec)
	     `(,it))))))

(defmethod generate-on-will-update ((spec observer-cell-spec))
  (with-slots (notify-will-update) spec
    `(progn
       (,notify-will-update))))

(defmethod generate-on-update ((spec observer-cell-spec))
  (with-slots (notify-update) spec
    `(progn
       (,notify-update))))

(defmethod generate-pre-update ((spec observer-cell-spec)) nil)
(defmethod generate-post-update ((spec observer-cell-spec)) nil)

(defmethod generate-cell-definition ((spec observer-cell-spec))
  (with-slots (stale?
	       updating?
	       will-update
	       notify-update
	       notify-will-update
	       update)
      spec

    `(progn
       (defvar ,stale? t)
       (defvar ,updating? nil)

       (declaim (ftype function ,notify-update ,notify-will-update))

       ,(call-next-method))))

(defmethod generate-extra ((spec observer-cell-spec))
  (with-slots (will-update update) spec
    (with-gensyms (arg)
      `(progn
	 ,(call-next-method)
	 (defun ,will-update (,arg)
	   (declare (ignorable ,arg))
	   ,(generate-will-update spec arg))

	 (defun ,update (,arg)
	   (declare (ignorable ,arg))
	   ,(generate-update spec arg))))))
