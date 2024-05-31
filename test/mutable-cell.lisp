;;;; Test suite for mutable cells

(defpackage :live-cells/test-mutable-cell
  (:use
   :generic-cl
   :live-cells
   :fiveam
   :live-cells/test))

(in-package :live-cells/test-mutable-cell)

(def-suite mutable-cell
  :description "Test basic mutable cells holding a non-computed value."
  :in live-cells)

(in-suite mutable-cell)


;;; Tests

(test initial-value
  "Test that value of cell equals initial value given"

  (cell-let ((cell 10))
    (is (= 10 cell))))

(test set-value
  "Test that setting the value of a mutable cell changes its value"

  (cell-let ((cell 15))
    (setf cell 23)
    (is (= 23 cell))))

(test multiple-set-value
  "Test that the cell keeps the latest value that was set"

  (cell-let ((cell 15))
    (setf cell 23)
    (setf cell 101)

    (is (= 101 cell))))

(test set-calls-observers
  "Test that setting the value of a mutable cell results in its observers being called"

  (cell-let ((cell 15))
    (with-expected-values (cell) (23)
      (setf cell 23))))

(test multiple-set-calls-observers
  "Test that the observers of a mutable cell are called every time its value is set"

  (cell-let ((cell 15))
    (with-expected-values (cell) (23 101)
      (setf cell 23)
      (setf cell 101))))

(test observer-removal
  "Test that the observer of a mutable cell is not called after it is removed"

  (cell-let ((cell 15))
    (with-expected-values (cell stop) (23)
      (setf cell 23)
      (stop)
      (setf cell 101))))

(test set-equal-value
  "Test that observers are not called if the new value of the cell is equal to its previous value"

  (cell-let ((cell 56))
    (with-expected-values (cell) ()
      (setf cell 56))))

(test all-observers-called
  "Test that all mutable cell observers are called when value changes"

  (cell-let ((cell 3))
    (with-expected-values (cell) (5 8 12)
      (setf cell 5)

      (with-expected-values (cell) (8 12)
        (setf cell 8)
        (setf cell 12)))))

(test batch-updates
  "Test that batch updates work correctly"

  (cell-let ((a 0)
             (b 0)
             (op "")

             (sum (+ a b))
             (msg (format nil "~a ~a ~a = ~a" a op b sum)))

    (with-expected-values (msg)
        ("1 + 2 = 3"
         "10 plus -3 = 7")

      (batch
        (setf a 1)
        (setf b 2)
        (setf op "+"))

      (batch
        (setf a 10)
        (setf b -3)
        (setf op "plus")))))
