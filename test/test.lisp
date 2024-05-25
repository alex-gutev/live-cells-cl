;;;; Defines the master test suite for live-cells

(in-package :live-cells/test)

(def-suite live-cells
  :description "Master test suite for live-cells.")

(defun test-live-cells ()
  "Run all the unit tests"

  (run! 'live-cells))



;;; Utilities

(defstruct test-value-observer
  "An observer that records the values of a cell

VALUES is an array containing the value of the cell at each call to
the observer.

STOP is a function of no arguments, which stops the observer when it
is called."

  values
  stop)

(defmacro observe-values (cell)
  "Create a TEST-VALUE-OBSERVER that records all the values produced by CELL.

A value is recorded in the TEST-VALUE-OBSERVER whenever the value of
CELL changes."

  (with-gensyms (first? cell-values value)
    `(let ((,first? t)
           (,cell-values (make-array 0 :adjustable t :fill-pointer t)))

      (make-test-value-observer
       :values ,cell-values
       :stop (live
               (let ((,value ,cell))
                 (unless ,first?
                   (setf ,cell-values
                         (nconcatenate ,cell-values (list ,value))))

                 (setf ,first? nil)))))))

(defmacro with-observed-values (cell (values-sym &optional stop-sym) &body body)
  "Record the value produced by CELL in a vector.

VALUES-SYM is the symbol naming the variable to which the vector,
holding the values produced by CELL, is bound. STOP-SYM is the symbol
naming the function, which, when called, stops the recording of
values. After this function is called further values produced by CELL
will not be accumulated in VALUES-SYM.

Both the variable named by VALUES-SYM, and the function named by
STOP-SYM are made available to the forms in BODY."

  (with-gensyms (observer)
    `(let ((,observer (observe-values ,cell)))
       (unwind-protect
            (symbol-macrolet ((,values-sym (test-value-observer-values ,observer)))
              (flet ,(when stop-sym
                       `((,stop-sym () (funcall (test-value-observer-stop ,observer)))))
                ,@body))

         (funcall (test-value-observer-stop ,observer))))))

(define-condition test-cell-error (error)
  ()

  (:documentation
   "Used for testing conditions signaled/raised inside computed cells."))


;;; Lifecycle test cell

(defstruct lifecycle-counter
  "Counts the number of times INIT and PAUSE have been called for a cell.

INIT is the number of times INIT has been called, and PAUSE is the
number of times PAUSE has been called."

  (init 0)
  (pause 0))

(defclass lifecycle-counter-cell (cell-spec)
  ((lifecycle-counter
    :initarg :counter
    :documentation
    "Form evaluating to a `LIFECYCLE-COUNTER'"))

  (:documentation
   "A cell that counts the number of times it has been initialized and paused."))

(defmethod generate-init ((spec lifecycle-counter-cell))
  (with-slots (lifecycle-counter) spec
    `(progn
       ,(call-next-method)
       (incf (lifecycle-counter-init ,lifecycle-counter)))))

(defmethod generate-pause ((spec lifecycle-counter-cell))
  (with-slots (lifecycle-counter) spec
    `(progn
       (incf (lifecycle-counter-pause ,lifecycle-counter))
       ,(call-next-method))))

(defmacro lifecycle-test-cell ((name value counter) &body body)
  "Create a cell that counts how many times it has been initialized and paused.

NAME is the name of the symbol to which the cell is bound. This is
made available to the forms in BODY.

VALUE is the constant value of the cell.

COUNTER is a form that evaluates to a `LIFECYCLE-COUNTER'. This
counter is used to keep track of how many times the cell has been
initialized and paused."

  (with-gensyms (counter-var)
    `(let ((,counter-var ,counter))
       ,(-> (make-instance
              'lifecycle-counter-cell
              :name name
              :counter counter-var
              :init value)

             (make-local-cell-definition%
              body)))))
