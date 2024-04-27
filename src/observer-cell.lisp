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

   (changed-dependencies
    :initarg :changed-dependencies
    :initform (cell-symbol 'cell 'changed-dependencies)
    :accessor changed-dependencies
    :documentation
    "Name of the variable holding the number of dependencies that were
    changed during the current update cycle.")

   (did-change?
    :initarg :did-change?
    :initform (cell-symbol 'cell 'did-change?)
    :accessor did-change?
    :documentation
    "Name of the variable holding the flag for whether the cell's
    value may have changed during the current update cycle.")

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

(defgeneric generate-on-update (spec did-change)
  (:documentation
   "Generate the form which calls the 'update' function of the observers of the cell defined by SPEC.

DID-CHANGE is the symbol identifying the variable holding the value of
the 'did change' argument to the update call."))

(defgeneric generate-will-update (spec arg)
  (:documentation
   "Generate the 'will-update' function of the cell defined by SPEC.

The return value should be a single form implementing the function,
with ARG being the symbol to use for the function argument."))

(defgeneric generate-update (spec arg did-change)
  (:documentation
   "Generate the 'update' function of the cell defined by SPEC.

The return value should be a single form implementing the function,
with ARG being the symbol to use for the function argument.

DID-CHANGE is the symbol identifying the variable holding the value of
the 'did change' argument to the update call."))

(defgeneric generate-did-change? (spec)
  (:documentation
   "Generate an expression which checks whether the value of the cell defined by SPEC has changed during the last update cycle.

The expression should evaluate to true if the value of the cell may
have changed, even if it has not necessarily changed, and should
evaluate to NIL if it is known with certainty that the value of the
cell has not changed."))


;;; Implementations

(defmethod generate-will-update ((spec observer-cell-spec) arg)
  (with-slots (updating? stale? changed-dependencies did-change?) spec
    `(progn
       (when (not ,updating?)
         (assert (zerop ,changed-dependencies)
                 (,changed-dependencies)
                 (concatenate
                  "The number of changed dependencies is not equal zero at the start of the update cycle."
                  "~%~%"
                  "This indicates that one of the cell's dependencies missed an 'update' call.~%~%"
                  "Unless this error originates from third-party code, this indicates a bug in live-cells-cl."))

         ,@(awhen (generate-pre-update spec)
             `(,it))

         (setf ,updating? t
               ,did-change? nil
               ,changed-dependencies 0)

         ,@(awhen (generate-on-will-update spec)
             `(,it))

         (setf ,stale? t))

       (incf ,changed-dependencies))))

(defmethod generate-update ((spec observer-cell-spec) arg changed)
  (with-slots (updating? stale? changed-dependencies did-change?) spec
    `(progn
       (when ,updating?
         (assert (plusp ,changed-dependencies)
                 (,changed-dependencies)
                 (concatenate
                  "More 'update' calls than 'will-update' calls in the current update cycle.~%~%"
                  "The number of 'update' calls must match exactly the number of 'will-update' calls"
                  " in a given update cycle.~%~%"
                  "Unless this error originates from third-party code, this indicates a bug in live-cells-cl."))

         (setf ,did-change? (or ,did-change? ,changed))

         (when (zerop (decf ,changed-dependencies))
           (setf, stale? (or ,stale? ,did-change?))

           ,@(with-gensyms (changed)
               (awhen (generate-on-update spec changed)
                 `((let ((,changed (and ,did-change? ,(generate-did-change? spec))))
                     ,it))))

           (setf ,updating? nil)

           ,@(awhen (generate-post-update spec)
               `((when ,did-change?
                   ,it))))))))

(defmethod generate-on-will-update ((spec observer-cell-spec))
  (with-slots (notify-will-update) spec
    `(progn
       (,notify-will-update))))

(defmethod generate-on-update ((spec observer-cell-spec) did-change)
  (with-slots (notify-update) spec
    `(progn
       (,notify-update :did-change ,did-change))))

(defmethod generate-pre-update ((spec observer-cell-spec)) nil)
(defmethod generate-post-update ((spec observer-cell-spec)) nil)

(defmethod generate-did-change? ((spec observer-cell-spec)) t)

(defmethod generate-cell-variables ((spec observer-cell-spec))
  (with-slots (stale? updating? changed-dependencies did-change?) spec

    (list*
     (make-variable-spec
      :name stale?
      :initform t
      :type :variable)

     (make-variable-spec
      :name updating?
      :initform nil
      :type :variable)

     (make-variable-spec
      :name changed-dependencies
      :initform 0
      :type :variable)

     (make-variable-spec
      :name did-change?
      :initform nil
      :type :variable)

     (call-next-method))))

(defmethod generate-cell-functions ((spec observer-cell-spec))
  (with-slots (will-update update) spec
    (with-gensyms (arg changed)
      (append
       (call-next-method)

       (list
        (make-function-spec
         :name will-update
         :type :function
         :lambda-list `(,arg)
         :body (list (generate-will-update spec arg)))

        (make-function-spec
         :name update
         :type :function
         :lambda-list `(,arg ,changed)
         :body (list (generate-update spec arg changed))))))))
