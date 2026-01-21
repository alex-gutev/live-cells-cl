Cell Value Forms
================

This page goes over some tools that can be used when writing the
*value forms* of cells.

Aborting a computation
----------------------

The computation of a computed cell's value can be aborted using the
:cl:function:`NONE` function. When :cl:function:`NONE` is called
inside a computed cell, the value form is exited and the cell's
current value is preserved. This can be used to prevent a cell’s value
from being recomputed when a condition is not met.

.. code-block::

   (defcell a 4)
   (defcell b (if (< a 10) a (none)))

   (setf a 6)
   (pprint b) ; Prints 6
   
   (setf a 15)
   (pprint b) ; Prints 6
   
   (setf a 8)
   (pprint b) ; Prints 8

In this example the computed cell ``B`` evaluates to the value of
``A`` if it is less than ``10``. Otherwise its current value is
preserved with ``(NONE)``. When the value of ``A`` is set to ``15``,
``B`` keeps its existing value ``8``.

.. tip::

   The symbol :cl:symbol:`NONE` is equivalent to calling
   :cl:function:`NONE` with no arguments. This definition of ``B`` can
   be rewritten as:

   .. code-block::

      (defcell b (if (< a 10) a none))

If :cl:function:`NONE` is called while computing the initial value of
the cell, the cell is initialized to the value provided in the
optional argument to :cl:function:`NONE`, which defaults to :cl:symbol:`COMMON-LISP:NIL` if
no argument is given.

The following definition of ``B`` ensures that its value is
initialized to ``1``, rather than :cl:symbol:`COMMON-LISP:NIL`, if the
initial value of ``A`` is greater than or equal to ``10``.

.. code-block::

   (defcell b
     (if (< a 10)
         a
	 (none 1)))

.. attention::

   The value of a *computed cell* is only computed if it is actually
   referenced. :cl:function:`NONE` only preserves the current value of
   the cell, but this might not be the latest value of the cell if the
   cell is only referenced conditionally. A good rule of thumb is to
   use :cl:function:`NONE` only to prevent a cell from holding an
   invalid value.
   
Condition handling
------------------

Conditions signaled during the computation of a cell's value are
resignaled when the cell's value is referenced. This allows conditions
to be handled in cells directly using :cl:macro:`HANDLER-BIND` and
:cl:macro:`HANDLER-CASE`.


.. code-block::

   (defcell text "0")
   (defcell n (parse-integer text))

   (defcell validp
     (handler-case (plusp n)
       (parse-error () nil)))

   (live
     (if validp
         (format t "Valid~%")
	 (format t "The input ~w is invalid~%" text)))

In this example, cell ``N`` parses an integer from the string held in
cell ``TEXT``. ``VALIDP`` is a computed cell that evaluates to true if
the parsed integer, held in ``N``, is greater than ``0``. ``VALIDP``
evaluates to :cl:symbol:`COMMON-LISP:NIL` if ``N`` is less than ``0``
or an integer could not be parsed from the string held in ``TEXT``,
that is a :cl:symbol:`COMMON-LISP:PARSE-ERROR` condition was signaled
while computing the value of ``N``.

The following assignments:

.. code-block::

   (setf text "3")
   (setf text "-1")
   (setf text "not a number")
   (setf text "1")

result in the following being printed:

.. code-block:: text

   Valid
   The input "-1" is invalid
   The input "not a number" is invalid
   Valid

.. note::

   This example also demonstrates that

   * Live blocks can reference cells conditionally. In this case
     cell ``TEXT`` is only referenced when ``VALIDP`` is false.

   * Computed cells, ``VALIDP`` in this example, can be observed just
     like any other cell. In-fact, the live block doesn't care whether
     a cell is computed cell or not.

Peeking Cells
-------------

If you want to use the value of a cell in a computed cell but don’t
want changes in the cell's value triggering a recomputation, wrap the
cell reference in :cl:macro:`PEEK`.

.. code-block::

   (defcell a 0)
   (defcell b 1)

   (defcell c (+ a (peek b)))

   (live
     (format t "~a%" c))

   (setf a 3) ; Prints 4
   (setf b 5) ; Doesn't print anything
   (setf a 7) ; Prints 7

In this example ``c`` is a computed cell that references the value of
``a`` and *peeks* the value of ``b``. Changing the value of ``a``
causes the value of ``c`` to be recomputed, and hence the *live* block
is run. However, changing the value of ``b`` does not cause the value
of ``c`` to be recomputed because ``b`` is only referenced within a
:cl:macro:`PEEK` form.


