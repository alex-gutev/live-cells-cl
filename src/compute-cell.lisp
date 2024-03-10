;;; Provides the definitions for building a stateless computed cell.

(in-package :live-cells.base)

(defstruct compute-cell
  "A stateless cell that computes a value as a function of one or more argument cells.

The value of this cell is computed by calling the function COMPUTE,
i.e. (FUNCALL COMPUTE). COMPUTE is called whenever CELL-VALUE is
called on this cell.

Observers added to this cell are directly added as observers of the
cells in ARGUMENTS. Thus this cell does not actually keep track of
observers, nor does it store its value, but delegates the task to the
cells in ARGUMENTS."

  arguments
  compute)

(defmethod cell-value ((cell compute-cell))
  (funcall (compute-cell-compute cell)))

(defmethod add-observer ((cell compute-cell) observer)
  (doseq (arg (compute-cell-arguments cell))
    (add-observer arg observer)))

(defmethod remove-observer ((cell compute-cell) observer)
  (doseq (arg (compute-cell-arguments cell))
    (remove-observer arg observer)))


;;; Argument Tracker

(defvar *track-cell-callback* nil
  "Callback function that is called when a cell is used by USE-CELL.

This is a function of one argument that is called whenever USE-CELL is
called. The cell passed to USE-CELL is passed to this callback.")

(defmacro with-tracker (((cell-arg) &body body) &body forms)
  "Evaluate FORMS with a cell argument tracker in effect.

The FORMS are evaluated in a context where *TRACK-CELL-CALLBACK* is
bound to a lexical function defined by BODY. The forms in BODY are
evaluated whenever USE-CELL is called (within FORMS) with CELL-ARG
being the name of the symbol which is bound to the cell that was
passed to USE-CELL."

  `(let ((*track-cell-callback*
	   (lambda (,cell-arg) ,@body)))
     ,@forms))

(defun use-cell (cell)
  "Get the value of a cell and register it as a dependency.

This function returns the value of CELL, by (CELL-VALUE CELL), but
also calls the function bound to *TRACK-CELL-CALLBACK* on CELL, if it
is non-null."

  (when *track-cell-callback*
    (funcall *track-cell-callback* cell))

  (cell-value cell))
