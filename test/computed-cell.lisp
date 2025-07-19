;;;; Tests suite for computed cells

(defpackage :live-cells/test-computed-cell
  (:use
   :generic-cl
   :live-cells
   :fiveam
   :live-cells/test))

(in-package :live-cells/test-computed-cell)

;;; Test Suite Definition

(def-suite computed-cell
  :description "Test cells with a value that is a function of other cells."
  :in live-cells)

(in-suite computed-cell)

;;; Tests

(test initial-value
  "Test that the initial value equals result of cell expression"

  (cell-let ((a 1)
             (b (+ a 1)))

    (is (= 2 b))))

(test recompute-on-argument-change
  "Test that the value of the cell is reevaluated when the value of the argument cell changes"

  (cell-let ((a 1)
             (b (+ a 1)))

    (with-expected-values (b) (6)
      (setf a 5)

      (is (= 6 b)))))

(test recompute-on-1st-argument-change
  "Test that the value of the cell is reevaluated when the value of the 1st argument cell changes"

  (cell-let ((a 1)
             (b 2)
             (c (+ a b)))

    (with-expected-values (c) (9)
      (setf a 7)

      (is (= 9 c)))))

(test recompute-on-2nd-argument-change
  "Test that the value of the cell is reevaluated when the value of the 2nd argument cell changes"

  (cell-let ((a 1)
             (b 2)
             (c (+ a b)))

    (with-expected-values (c) (12)
      (setf b 11)

      (is (= 12 c)))))

(test observers-notified-when-arguments-change
  "Test that the observers of the cell are notified every time the value of an argument cell changes"

  (cell-let ((a 1)
             (b 2)
             (c (+ a b)))

    (with-expected-values (c) (10 12 110)
      (setf a 8)
      (setf a 10)
      (setf b 100))))

(test observer-removal
  "Test that the observer of the cell is not called after it is removed."

  (cell-let ((a 1)
             (b 2)
             (c (+ a b)))

    (with-expected-values (c stop) (10)
      (setf a 8)
      (stop)

      (setf b 10)
      (setf a 100))))

(test multiple-observers
  "Test that all observers are called when the value of the cell changes"

  (cell-let ((a 1)
             (b 2)
             (c (+ a b)))

    (with-expected-values (c) (10 18 110)
      (setf a 8)

      (with-expected-values (c) (18 110)
        (setf b 10)
        (setf a 100)))))

(test conditional-arguments
  "Test computed cells with conditionally referenced arguments"

  (cell-let ((a t)
             (b 2)
             (c 3)
             (d (if a b c)))

    (with-expected-values (d) (1 3 10)
      (setf b 1)
      (setf a nil)
      (setf c 10))))

(test computed-arguments
  "Test computed cells with arguments that are computed cells"

  (cell-let ((a t)
             (b 2)
             (c 3)
             (d (if a b c))
             (e 0)
             (f (+ d e)))

    (with-expected-values (f) (1 11 13 20)
      (setf b 1)
      (setf e 10)
      (setf a nil)
      (setf c 10))))

(test error-on-init
  "Test that conditions signaled during initial value computation are reproduced on access"

  (cell-let ((cell (error 'test-cell-error)))
    (signals test-cell-error cell)))

(test error-on-init-observed
  "Test that conditions signaled during initial value computation are reproduced on access while observed"

  (cell-let ((cell (error 'test-cell-error)))
    (with-observed-values cell (vs)
      (signals test-cell-error cell))))

(test none-preserves-value
  "Test that using NONE preserves the value of the cell."

  (cell-let ((a 0)
             (evens (if (evenp a) a none)))

    (with-expected-values (evens) (2 4)
      (setf a 1)
      (setf a 2)
      (setf a 3)
      (setf a 4)
      (setf a 5))))

(test none-default-value
  "Test using NONE while given a default value"

  (cell-let ((a 1)
             (evens (if (evenp a) a (none 100))))

    (with-expected-values (evens) (4 6)
      (is (= 100 evens))

      (setf a 3)
      (setf a 4)

      (setf a 5)
      (is (= 4 evens))

      (setf a 6))))

(test none-first-value
  "Test using NONE when computing the first value of the cell"

  (cell-let ((a 1)
             (evens (if (evenp a) a none)))

    (with-expected-values (evens) (4 6)
      (is (null evens))

      (setf a 3)
      (setf a 4)
      (setf a 5)
      (setf a 6))))
