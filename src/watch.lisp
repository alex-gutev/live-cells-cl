(in-package :live-cells.base)

(defstruct cell-watcher
  "Maintains the state of a watch function.

CALLBACK is called whenever the values of the cells referenced within
it change.

UPDATING is true if the values of the argument cells are currently
being updated.

ARGUMENTS is the set of cells observed by the watcher."

  callback
  (updating nil)
  (arguments (make-hash-set)))

(defun watch-cells (callback)
  "Register a CALLBACK to be called when the values of cells change.

CALLBACK is called once, before this function returns, to determine
which cells it is observing. Afterwards CALLBACK is called whenever
the values of the cells referenced within it change.

NOTE: CALLBACK can conditionally reference cells, they will be tracked
correctly.

Returns a `CELL-WATCHER' object, that allows the CALLBACK to be
deregistered using UNWATCH."

  (let ((watcher (make-cell-watcher :callback callback)))
    (track-referenced-cells watcher callback)
    watcher))

(defun unwatch (watcher)
  "Deregister the callback registered in WATCHER.

When this function is called on a `CELL-WATCHER', previously returned
by WATCH-CELLS, the CALLBACK function that is registered will no
longer be called when the values of the cells referenced within it
change."

  (with-accessors ((arguments cell-watcher-arguments)) watcher
    (doseq (arg arguments)
      (remove-observer arg watcher))

    (clear arguments)))

;;; Internal

(defun track-referenced-cells (watcher callback)
  "Call CALLBACK and begin observing the cells that were referenced.

All cells that are referenced by CALLBACK are added to the ARGUMENTS
of WATCHER, and subsequently WATCHER is added as an observer.

The value returned by CALLBACK is returned by this function."

  (with-accessors ((arguments cell-watcher-arguments)) watcher
    (with-tracker
	((cell)
	  (unless (memberp cell arguments)
	    (nadjoin cell arguments)
	    (add-observer cell watcher)))

      (funcall callback))))

(defmethod will-update ((watcher cell-watcher) cell)
  (with-accessors ((updating cell-watcher-updating))
      watcher

    (unless updating
      (setf updating t))))

(defmethod update ((watcher cell-watcher) cell)
  (with-accessors ((updating cell-watcher-updating)
		   (callback cell-watcher-callback))
      watcher

    (when updating
      (setf updating nil)
      (track-referenced-cells watcher (funcall callback)))))

;;TODO: Move this to the public package

(defmacro! live (&body forms)
  "Evaluate FORMS whenever the cells referenced within them change.

FORMS are evaluated once initially (in an implicit PROGN) with the
value of the last form returned. Subsequently, FORMS are evaluated
again whenever the values of the cells referenced within FORMS change.

Currently this 'watch function' is established permanently meaning
there is no way to stop FORMS from being called."

  `(let* ((,g!callback (lambda () ,@forms))
	  (,g!watcher (make-cell-watcher :callback ,g!callback)))
     (init-watcher ,g!watcher ,g!callback)))
