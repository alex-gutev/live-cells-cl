;;;; Provides functionality for reading the value of a cell without
;;;; reacting to changes in the value

(in-package :live-cells)

(defmacro peek (form)
  "Read the value of cells without triggering a recomputation.

FORM is evaluated in a context such that when the value of a cell is
referenced it is not registered as an argument cell of the cell in
which the PEEK form is evaluated. This means that the value of the
cell, in which the PEEK form is evaluated, will not be recomputed when
the values of the cells referenced in FORM change.

In the following example

.. code-block::

   (defcell a 0)
   (defcell b 1)

   (defcell sum (+ a (peek b))

the SUM cell references the value of A directly and *peeks* the value
of B (using PEEK). Changing the value of A will cause the value of the
SUM cell to be recomputed. However, changing the value of B will not
cause the value of SUM to be computed.

.. important::

   This macro has no effect if CELL is referenced outside a PEEK form
   within the same cell definition.

   .. code-block::

      (defcell sum (+ b (peek b))

   In this example, PEEK has no effect because the SUM cell references
   B outside a PEEK form.

"

  (with-gensyms (arg-cell
                 did-change
                 observer-arg
                 tracker
                 update
                 will-update)

    (let* ((key ``(peek ,(argument-key ,arg-cell)))
           (observer-record
             `(make-observer
               :key ,key
               :will-update #',will-update
               :update #',update)))

      `(flet ((,update (,arg-cell ,did-change)
                (declare (ignore ,arg-cell ,did-change)))
              (,will-update (,arg-cell)
                (declare (ignore ,arg-cell))))

         (let ((,tracker *track-cell-callback*))
           (with-tracker
               ((,arg-cell)
                 (let ((*track-cell-callback* ,tracker))
                   (track-argument
                    (make-argument
                     :key ,key
                     :add-observer (lambda (,observer-arg)
                                     (declare (ignore ,observer-arg))
                                     (call-add-observer ,arg-cell ,observer-record))
                     :remove-observer (lambda (,observer-arg)
                                        (declare (ignore ,observer-arg))
                                        (call-remove-observer ,arg-cell ,observer-record))))))

             ,form))))))
