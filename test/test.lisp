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
