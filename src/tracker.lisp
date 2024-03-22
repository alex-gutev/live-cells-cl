;;;; Provides functionality for tracking argument cells

(in-package :live-cells)

;;; Argument Tracker

(defvar *track-cell-callback* nil
  "Callback function that is called when a cell is used as an argument.

This is a function of one argument that is called whenever a use cell
function is called. The cell passed to the use cell function is passed
to this callback.")

(defmacro with-tracker (((cell-arg) &body body) &body forms)
  "Evaluate FORMS with a cell argument tracker in effect.

The FORMS are evaluated in a context where *TRACK-CELL-CALLBACK* is
bound to a lexical function defined by BODY. The forms in BODY are
evaluated whenever a use cell function called (within FORMS) with CELL-ARG
being the name of the symbol which is bound to the cell that was
passed to the use cell function."

  `(let ((*track-cell-callback*
	   (lambda (,cell-arg) ,@body)))
     ,@forms))

(defmacro! without-tracker (&body forms)
  "Evaluate FORMS without an argument cell tracker in-effect.

As a result calling a use cell function, within FORMS, will not call
*TRACK-CELL-CALLBACK*.

The value of the last form in FORMS is returned."

  `(with-tracker ((,g!arg) (declare (ignore ,g!arg)))
     ,@forms))

(defun track-argument (arg)
  "Register the cell identified by the `ARGUMENT' record ARG as an dependency.

This function calls the function bound to *TRACK-CELL-CALLBACK* on
ARG, if it is non-null."

  (when *track-cell-callback*
    (funcall *track-cell-callback* arg)))
