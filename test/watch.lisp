;;; Test suite checking that watch functions function correctly.

(defpackage :live-cells/test-watch
  (:use
   :generic-cl
   :live-cells
   :fiveam
   :live-cells/test))

(in-package :live-cells/test-watch)

;;; Test Suite Definition

(def-suite watch
  :description "Test watch functions"
  :in live-cells)

(in-suite watch)

;;; Tests

(test called-on-registration
  "Test that the watch function is called once on registration"

  (cell-let ((a 1)
             (b 2))

    (with-live-scope
      (with-watch-values (results collect)
        (live
          (collect (list a b)))

        (is (= #((1 2)) results))))))

(test called-when-cell-values-change
  "Test that the watch function is called when the values of the referenced cells change"

  (cell-let ((a 1)
             (b 2))

    (with-live-scope
      (with-watch-values (results collect)
        (live
          (collect (list a b)))

        (setf a 5)
        (setf b 10)

        (is (= #((1 2) (5 2) (5 10)) results))))))

(test called-during-batch-update
  "Test that the watch function is called during a batch cell update"

  (cell-let ((a 1)
             (b 2))

    (with-live-scope
      (with-watch-values (results collect)
        (live
          (collect (list a b)))

        (batch
          (setf a 5)
          (setf b 10))

        (batch
          (setf a 50)
          (setf b 80))

        (is (= #((1 2) (5 10) (50 80)) results))))))

(test conditional-reference
  "Test that conditionally referenced cells are watched correctly"

  (cell-let ((a 1)
             (b 2)
             (select t))

    (with-live-scope
      (with-watch-values (results collect)
        (live
          (collect (if select a b)))

        (setf a 3)
        (setf select nil)
        (setf b 5)

        (is (= #(1 3 2 5) results))))))

(test stop-watch-function
  "Test that the watch function is not called after it is stopped"

  (cell-let ((a 1)
             (b 2))

    (with-watch-values (results collect)
      (with-live-scope
        (live
          (collect (list a b)))

        (setf a 5)
        (setf b 10))

      (setf b 100)
      (setf a 30)

      (is (= #((1 2) (5 2) (5 10)) results)))))

(test init-and-pause
  "Test that INIT is called on registration and PAUSE called when stopped"

  (let ((counter (make-lifecycle-counter)))
    (lifecycle-test-cell (cell 1 counter)
      (with-live-scope
        (live (list cell))

        (is (= 1 (lifecycle-counter-init counter)))
        (is (= 0 (lifecycle-counter-pause counter))))

      (is (= 1 (lifecycle-counter-init counter)))
      (is (= 1 (lifecycle-counter-pause counter))))))

(test pause-not-called-before-all-watchers-stopped
  "Test that PAUSE is not called before every watch function has been stopped"

  (let ((counter (make-lifecycle-counter)))
    (lifecycle-test-cell (cell 1 counter)
      (with-live-scope
        (live (vector cell))

        (with-live-scope
          (live (list cell))

          (is (= 1 (lifecycle-counter-init counter)))
          (is (= 0 (lifecycle-counter-pause counter))))

        (is (= 1 (lifecycle-counter-init counter)))
        (is (= 0 (lifecycle-counter-pause counter))))

      (is (= 1 (lifecycle-counter-init counter)))
      (is (= 1 (lifecycle-counter-pause counter))))))
