Cells
=====

A cell is a container for a value that can be observed by one or more
observers, which react to changes in the value. You'll see exactly what
that means in a moment.

Global cells can be defined with :cl:macro:`DEFCELL`:

.. code-block::

   (defcell a-cell 1)

The first argument ``a-cell`` is a symbol naming the cell and the
second argument is the *value form* that computes the value of the
cell, the constant ``1`` in this case.

The value of a cell is referenced by the symbol identifying it:

.. code-block::

   (pprint a-cell) ; Prints 1


So far cells look just like variables.

Setting the value of a cell
---------------------------

The value of a cell can be set using ``SETF`` just like a variable.

.. code-block::

   (defcell a 1)

   (setf a 2)
   (pprint a) ; Prints 2

.. attention::

   The value of a cell can only be set if its *value form* is a
   compile time constant. Otherwise, it is assumed to be a *computed
   cell*.
   
Computed Cells
--------------

If the *value form* of a cell references another cell, a *computed cell*
is created. The value of a *computed cell* is recomputed whenever the
values of any of the *argument cells*, referenced in the *value form*,
change.

.. code-block::

   (defcell a 1)
   (defcell b 2)

   ;; SUM is a computed cell referencing cells A and B
   (defcell sum (+ a b))

   (pprint sum) ; Prints 3

   (setf a 5)
   (pprint sum) ; Prints 5

   (setf a 10)
   (pprint sum) ; Prints 15

In this example ``SUM`` is a computed cell that references cells ``A``
and ``B``. Whenever the value of either ``A`` or ``B`` is changed, the
value of ``SUM`` is recomputed automatically. This distinguishes cells
from ordinary variables which keep the same value they are initialized
with until it is explicitly changed with ``SETF``.

Argument cells may also be referenced indirectly by a function
called from the *value form*.

.. code-block::

   (defcell delta 1)

   ;; Inc references cell DELTA
   (defun inc (n)
     (+ n delta))

   (defcell a 5)

   ;; References both cell A and DELTA
   (defcell sum (inc a))

The ``SUM`` cell directly references cell ``A`` and indirectly
references cell ``DELTA``, through the function ``INC``. Therefore,
changing the value of ``DELTA``, will also result in the value of
``SUM`` being recomputed.

Example:

.. code-block::

   (setf a 6)
   (pprint sum) ; Prints 7

   (setf delta 3)
   (pprint sum) ; Prints 9


Observing Cells
---------------

A cell can be observed with the :cl:macro:`LIVE` macro, which takes
one or more forms, much like :cl:macro:`PROGN`, that are evaluated in
sequence and may reference one or more argument cells. When the value
of at least one argument cell changes, the entire *live block* is
reevaluated, similar to how the value of a computed cell is
recomputed.

.. code-block::

   (defcell a 1)
   (defcell b 2)

   ;; This LIVE block is run whenever the value of
   ;; either A or B changes
   
   (live
     (format t "A = ~a, B = ~a~%" a b))

In this example a *live block* is defined that prints the values of
cells ``A`` and ``B``. This block is evaluated once, when the ``LIVE``
form is first evaluated, which results in the following being printed:

.. code-block::

   A = 1, B = 2

The block is evaluated again whenever the value of either ``A`` or
``B`` changes.

The following:

.. code-block::

   (setf a 5)
   (setf b 10)

results in the following two lines being printed:

.. code-block:: text

   A = 5, B = 1
   A = 5, B = 10

Each :cl:macro:`LIVE` form creates a new *live block* that observes only those
cells referenced by the forms within it.

.. code-block::

   (defcell a 0)
   (defcell b 1)

   (live
     (format t "A = ~a, B = ~a~%" a b))

   (live
     (format t "A = ~a~%" a))

In this example two *live blocks* are created. The first *live block*
observes cells ``A`` and ``B`` while the second block only observes
cell ``A``.

An assignment to cell ``A``, such as:

.. code-block::

   (setf a 20)


causes both blocks to be evaluated resulting in the following lines
being printed (the order in which the lines are printed may vary):

.. code-block:: text

   A = 20, B = 1
   A = 20

An assignment to cell ``B``, such as:

.. code-block::

   (setf b 10)

causes only the first block to be evaluated, since the second block is
not observing cell ``B``. This results in only one line being printed:

.. code-block:: text

   A = 20, B = 10

Stopping Observers
~~~~~~~~~~~~~~~~~~

The :cl:macro:`LIVE` macro returns a function of zero arguments that,
when called, stops the *live block*. Once a *live block* is stopped it
is no longer run when the values of the cells referenced within it
change.

.. code-block::

   (defcell a 0)

   (let ((stop (live
                (format t "A = ~a~%"))))
		
     (setf a 1) ; Prints A = 1
     (setf a 2) ; Prints A = 2

     ;; Stop the live block
     (funcall stop)

     (setf a 3)) ; Doesn't print anything
   

In this example the *live block* is stopped after two assignments to
cell ``A``. This results in the following lines being printed:

.. code-block:: text

   A = 0
   A = 1
   A = 2

.. note::

   The first line ``A = 0`` is printed when the :cl:macro:`LIVE` form
   is first evaluated.
   
The third assignment ``(setf a 3)`` doesn't cause the block to run
because the stop function has been called.

Batch Updates
-------------

The values of multiple cells can be set simultaneously by wrapping the
assignments (the :cl:macro:`SETF` forms) in a :cl:macro:`BATCH`
form. :cl:macro:`BATCH`, like :cl:macro:`PROGN`, takes one or more
forms, which are evaluated in sequence:

The effect of this is that while the values of the cells are changed
as soon as the `SETF` forms are evaluated, the observers (*live
blocks* and *computed cells*) are only notified after the last form in
:cl:macro:`BATCH` has been evaluated.

.. code-block::

   (defcell a 0)
   (defcell b 1)

   (live
     (format t "A = ~a, B = ~a~%" a b))

   ;; Only prints: A = 15, B = 3
   (batch
     (setf a 15)
     (setf b 3))

In this example the values of ``A`` and ``B`` are set to ``15`` and
``3``, respectively, within :cl:macro:`BATCH`. This causes the *live
block*, which prints the values of ``A`` and ``B``, to run once after
the value of ``B`` is set to ``3``. The first assignment ``(setf a
15)`` **DOES NOT** cause the *live block* to run.

Only two lines are printed. The first line:

.. code-block:: text

   A = 0, B = 1

is printed when the :cl:macro:`LIVE` form is first evaluated, while
the second line:

.. code-block:: text

   A = 15, B = 3

is printed after the :cl:macro:`BATCH` form is evaluated.

:cl:macro:`BATCH` can be nested within another :cl:macro:`BATCH`,
however the observers of the cells, which have their values changed
within the batch, are only notified when exiting the outermost
:cl:macro:`BATCH`.

For example the following:

.. code-block::

   (batch
     (batch
       (setf a 20))

     (setf b 100))

only results in one line ``A = 20, B = 100`` being printed.
     
Local Cells
-----------

Cells local to a given lexical scope can be defined with
:cl:macro:`CELL-LET`. Like :cl:macro:`LET`, the first argument is a
list of bindings to establish followed by a list of body forms that
are evaluated in order.

.. attention::

   All cells are lexically scoped, including global cells defined with
   :cl:macro:`DEFCELL`. This differs from global variables, defined
   with :cl:macro:`DEFVAR` and :cl:macro:`DEPARAMETER`, which are
   dynamically scoped.

Each binding is of the form ``(CELL-NAME VALUE-FORM)`` where
``CELL-NAME`` is the name of the cell, which is made visible to
the body forms, and ``VALUE-FORM`` is the cell value form.

.. code-block::

   (cell-let ((a 1)
              (b 2)
	      (sum (+ a b)))
     (live
       (format t "A + B = ~a~%" sum))

     (setf a 3)
     ...)

In this example three local cells are defined ``A``, ``B`` and a
computed cell ``SUM``. These cells can be used just like global cells
defined with :cl:macro:`DEFCELL`, however they are only visible to the
body forms of the :cl:macro:`CELL-LET`.

.. important::

   Cells defined using :cl:macro:`CELL-LET` can reference cells
   defined earlier in the same :cl:macro:`CELL-LET`. This makes
   :cl:macro:`CELL-LET` similar to :cl:macro:`LET*` rather than
   :cl:macro:`LET`.
