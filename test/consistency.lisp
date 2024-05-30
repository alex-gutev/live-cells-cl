;;;; Test suite checking that cells are updated consistently and
;;;; without glitches

(defpackage :live-cells/test-consistency
  (:use
   :generic-cl
   :live-cells
   :fiveam
   :live-cells/test))

(in-package :live-cells/test-consistency)

;;; Test Suite Definition

(def-suite consistency
  :description "Test that cells are updated without glitches"
  :in live-cells)

(in-suite consistency)

;;; Tests

(test no-glitches-multi-argument-cells
  "Test that no glitches are observed with multi argument cells"

  (cell-let ((a 0)
             (sum (+ a 1))
             (prod (* a 8))
             (result (+ sum prod)))

    (with-observed-values result (vs)
      (setf a 2)
      (setf a 6)

      (is (= (vector (+ (+ 2 1) (* 2 8))
                     (+ (+ 6 1) (* 6 8)))
             vs)))))

(test no-glitches-multi-argument-cells-unequal-branches
  "Test that no glitches are observed when branches are unequal in multi argument cells"

  (cell-let ((a 0)
             (sum1 (+ a 1))
             (sum (+ sum1 10))
             (prod (* a 8))
             (result (+ sum prod)))

    (with-observed-values result (vs)
      (setf a 2)
      (setf a 6)

      (is (= (vector (+ (+ 2 1 10) (* 2 8))
                     (+ (+ 6 1 10) (* 6 8)))

             vs)))))

(test no-glitches-batch
  "Test that no glitches are observed when batching cell value changes"

  (cell-let ((a 0)
             (b 0)
             (op "")
             (sum (+ a b))
             (msg (format nil "~a ~a ~a = ~a" a op b sum)))

    (with-observed-values msg (vs)
      (batch
        (setf a 1)
        (setf b 2)
        (setf op "+"))

      (batch
        (setf a 5)
        (setf b 6)
        (setf op "plus"))

      (is (= #("1 + 2 = 3" "5 plus 6 = 11") vs)))))

(test observers-called-correct-number-of-times
  "Test that observers are not called more than is necessary"

  (cell-let ((a 1)
             (b 2)
             (sum (+ a b))
             (c (+ a sum))
             (d (+ sum 2)))

    (with-observed-values c (cs)
      (with-observed-values d (ds)
        (batch
          (setf a 2
                b 3))

        (batch
          (setf a 3
                b 2))

        (batch
          (setf a 10
                b 20))

        (is (= #(7 8 40) cs))
        (is (= #(7 32) ds))))))
