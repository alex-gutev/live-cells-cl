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

    (with-observed-values b (vs)
      (setf a 5)

      (is (= 6 b))
      (is (= #(6) vs)))))

(test recompute-on-1st-argument-change
  "Test that the value of the cell is reevaluated when the value of the 1st argument cell changes"

  (cell-let ((a 1)
             (b 2)
             (c (+ a b)))

    (with-observed-values c (vs)
      (setf a 7)

      (is (= 9 c))
      (is (= #(9) vs)))))

(test recompute-on-2nd-argument-change
  "Test that the value of the cell is reevaluated when the value of the 2nd argument cell changes"

  (cell-let ((a 1)
             (b 2)
             (c (+ a b)))

    (with-observed-values c (vs)
      (setf b 11)

      (is (= 12 c))
      (is (= #(12) vs)))))

(test observers-notified-when-arguments-change
  "Test that the observers of the cell are notified every time the value of an argument cell changes"

  (cell-let ((a 1)
             (b 2)
             (c (+ a b)))

    (with-observed-values c (vs)
      (setf a 8)
      (setf a 10)
      (setf b 100)

      (is (= #(10 12 110) vs)))))

(test observer-removal
  "Test that the observer of the cell is not called after it is removed."

  (cell-let ((a 1)
             (b 2)
             (c (+ a b)))

    (with-observed-values c (vs stop)
      (setf a 8)
      (stop)

      (setf b 10)
      (setf a 100)

      (is (= #(10) vs)))))

(test multiple-observers
  "Test that all observers are called when the value of the cell changes"

  (cell-let ((a 1)
             (b 2)
             (c (+ a b)))

    (with-observed-values c (vs1)
      (setf a 8)

      (with-observed-values c (vs2)
        (setf b 10)
        (setf a 100)

        (is (= #(10 18 110) vs1))
        (is (= #(18 110) vs2))))))

(test conditional-arguments
  "Test computed cells with conditionally referenced arguments"

  (cell-let ((a t)
             (b 2)
             (c 3)
             (d (if a b c)))

    (with-observed-values d (vs)
      (setf b 1)
      (setf a nil)
      (setf c 10)

      (is (= #(1 3 10) vs)))))

(test computed-arguments
  "Test computed cells with arguments that are computed cells"

  (cell-let ((a t)
             (b 2)
             (c 3)
             (d (if a b c))
             (e 0)
             (f (+ d e)))

    (with-observed-values f (vs)
      (setf b 1)
      (setf e 10)
      (setf a nil)
      (setf c 10)

      (is (= #(1 11 13 20) vs)))))

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

    (with-observed-values evens (vs)
      (setf a 1)
      (setf a 2)
      (setf a 3)
      (setf a 4)
      (setf a 5)

      (is (= #(2 4) vs)))))

(test none-first-value
  "Test using NONE when computing the first value of the cell"

  (cell-let ((a 1)
             (evens (if (evenp a) a none)))

    (with-observed-values evens (vs)
      (is (null evens))

      (setf a 3)
      (setf a 4)
      (setf a 5)
      (setf a 6)
      (is (= #(4 6) vs)))))
