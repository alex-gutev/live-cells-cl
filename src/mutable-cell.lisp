(in-package :live-cells)

(defclass mutable-cell-spec (cell-spec)
  ()

  (:documentation
   "Specification for a cell that can have its value set directly."))

(defmethod generate-setf-expansion ((spec mutable-cell-spec))
  (with-slots (value notify-will-update notify-update) spec
    (with-gensyms (new-value)
      (values
       t
       nil
       nil
       (list new-value)

       `(progn
          (unless (= ,value ,new-value)
            (,notify-will-update)
            (setf ,value ,new-value)

            (if *batch-in-effect-p*
                (add-to-batch #',notify-update)
                (,notify-update)))

          ,new-value)

       value))))

(defvar *batch-in-effect-p* nil
  "Is mutable cell batching in effect?")

(defvar *batch-list* nil
  "List of 'notify update' functions to call after current mutable cell batch.")

(defun add-to-batch (notify-update)
  "Add a NOTIFY-UPDATE, of a mutable cell, to the current *BATCH-LIST*.

The NOTIFY-UPDATE function is called after the current batch ends."

  (push notify-update *batch-list*))

(defun end-batch ()
  "End the current mutable cell batch and call all 'notify update' functions in *BATCH-LIST*"

  (foreach #'funcall *batch-list*))

(defmacro batch (&body forms)
  "Batch assignments to the values of mutable cells.

The forms in FORMS are evaluated, in an implicit PROGN, with mutable
cell assignment batching in effect. This means that when the value of
a mutable cell is set by SETF within the dynamic extent of the BATCH
form, the observers of the cell are only notified after the last form
in FORMS is evaluated, or the BATCH form is exited by a non-local exit
such as by RETURN-FORM. The effect of this is that the cells appear to
have their values changed simultaneously, to their observers, that the
values of the mutable cells are all set simultaneously.

.. note::

   When a BATCH form is nested in the dynamic extent of another BATCH
   form, the nested BATCH form has no effect other than to evaluate
   FORMS. The observers of the cells are only notified when exiting
   the outermost BATCH form.

Returns the value of the last form in FORMS."

  `(if *batch-in-effect-p*
       (progn ,@forms)

       (let ((*batch-in-effect-p* t)
             (*batch-list* nil))

         (unwind-protect (progn ,@forms)
           (end-batch)))))
