;;; Defines the master test suite for live-cells

(in-package :live-cells/test)

(def-suite live-cells
  :description "Master test suite for live-cells.")

(defun test-live-cells ()
  (run! 'live-cells))



;;; Utilities

(defstruct test-value-observer
  values
  stop)

(defmacro observe-values (cell)
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
  (with-gensyms (observer)
    `(let ((,observer (observe-values ,cell)))
       (unwind-protect
            (symbol-macrolet ((,values-sym (test-value-observer-values ,observer)))
              (flet ,(when stop-sym
                       `((,stop-sym () (funcall (test-value-observer-stop ,observer)))))
                ,@body))

         (funcall (test-value-observer-stop ,observer))))))
