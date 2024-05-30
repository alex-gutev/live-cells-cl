;;; Test suite checking that cells are initialized and cleaned up
;;; correctly.

(defpackage :live-cells/test-init-cleanup
  (:use
   :generic-cl
   :live-cells
   :fiveam
   :live-cells/test))

(in-package :live-cells/test-init-cleanup)

;;; Test Suite Definition

(def-suite init-cleanup
  :description "Test that cells are initialized and cleaned up correctly"
  :in live-cells)

(in-suite init-cleanup)

;;; Tests

(test init-not-called-before-add-observer
  "Test that INIT is not called until an observer is added."

  (let ((counter (make-lifecycle-counter)))
    (lifecycle-test-cell (cell 1 counter)
      (is (= 0 (lifecycle-counter-init counter))))))

(test init-called-on-first-add-observer
  "Test that INIT is called when the first observer is added."

  (let ((counter (make-lifecycle-counter)))
    (lifecycle-test-cell (cell 1 counter)
      (with-observed-values cell (vs1)
        (with-observed-values cell (vs2)
          (is (= 1 (lifecycle-counter-init counter))))))))

(test pause-called-when-all-observers-removed
  "Test that PAUSE is called only when all observers have been removed."

  (let ((counter (make-lifecycle-counter)))
    (lifecycle-test-cell (cell 1 counter)
      (with-observed-values cell (vs1)
        (with-observed-values cell (vs2)
          (is (= 1 (lifecycle-counter-init counter))))

        (is (= 1 (lifecycle-counter-init counter)))
        (is (= 0 (lifecycle-counter-pause counter))))

      (is (= 1 (lifecycle-counter-init counter)))
      (is (= 1 (lifecycle-counter-pause counter))))))

(test init-called-after-pause-on-add-observer
  "Test that INIT is called again when ADD-OBSERVER is called after all observers have been removed."

  (let ((counter (make-lifecycle-counter)))
    (lifecycle-test-cell (cell 1 counter)
      (with-observed-values cell (vs1)
        (with-observed-values cell (vs2)
          (is (= 1 (lifecycle-counter-init counter))))

        (is (= 1 (lifecycle-counter-init counter)))
        (is (= 0 (lifecycle-counter-pause counter))))

      (is (= 1 (lifecycle-counter-init counter)))
      (is (= 1 (lifecycle-counter-pause counter)))

      (with-observed-values cell (vs1)
        (is (= 2 (lifecycle-counter-init counter)))
        (is (= 1 (lifecycle-counter-pause counter))))

      (is (= 2 (lifecycle-counter-init counter)))
      (is (= 2 (lifecycle-counter-pause counter))))))
