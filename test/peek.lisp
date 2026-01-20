;;; Test suite checking that peeking cell values works correctly

(defpackage :live-cells/test-peek
  (:use
   :generic-cl
   :live-cells
   :fiveam
   :live-cells/test))

(in-package :live-cells/test-peek)

;;; Test Suite Definition

(def-suite peek
  :description "Test PEEK macro"
  :in live-cells)

(in-suite peek)

;;; Tests

(test peeked-value-equals-cell-value
  "Test that PEEK returns the actual value of the cell"

  (cell-let ((a 1)
             (b 2))
    (with-live-scope
      (with-watch-values (results collect)
        (live
          (collect (list a (peek b))))

        (setf b 3)
        (setf a 2)

        (is (= #((1 2) (2 3)) results))))))

(test peek-suppress-recomputation
  "Test that PEEK suppresses recomputation on changes to the referenced CELL"

  (cell-let ((a 0)
             (b 1)
             (c (list (peek a) b)))

    (with-expected-values (c)
        ('(3 5) '(3 10) '(2 13))

      (setf a 1)
      (setf a 2)
      (setf a 3)
      (setf b 5)
      (setf b 10)
      (setf a 2)
      (setf b 13))))

(test manage-same-observers
  "Test that observers added through PEEK are added to the same set."

  (let ((counter (make-lifecycle-counter)))
    (lifecycle-test-cell (a 1 counter)
      (flet ((p () (peek a)))
        (is (= 0 (lifecycle-counter-init counter)))

        (with-observed-values (p) (vs1)
          (with-observed-values (p) (vs2)
            (is (= 1 (lifecycle-counter-init counter))))
          (is (= 0 (lifecycle-counter-pause counter))))

        (is (= 1 (lifecycle-counter-pause counter)))))))

(test init-called-after-pause-on-add-observer
  "Test that INIT is called again when ADD-OBSERVER is called after all observers have been removed."

  (let ((counter (make-lifecycle-counter)))
    (lifecycle-test-cell (a 1 counter)
      (flet ((p () (peek a)))
        (is (= 0 (lifecycle-counter-init counter)))
        (is (= 0 (lifecycle-counter-pause counter)))

        (with-observed-values (p) (vs)
          (is (= 1 (lifecycle-counter-init counter)))
          (is (= 0 (lifecycle-counter-pause counter))))

        (is (= 1 (lifecycle-counter-pause counter)))

        (with-observed-values (p) (vs)
          (is (= 2 (lifecycle-counter-init counter)))
          (is (= 1 (lifecycle-counter-pause counter))))

        (is (= 2 (lifecycle-counter-pause counter)))))))
