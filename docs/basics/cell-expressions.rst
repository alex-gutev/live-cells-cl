Cell Expressions
================

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
``A`` if it is less than ``10``. Otherwise it's current value is
preserved with ``(NONE)``. When the value of ``A`` is set to ``15``,
``B`` keeps its existing value ``8``.

.. tip::

   The symbol :cl:symbol:`NONE` is equivalent to calling
   :cl:function:`NONE` with no arguments. The definition of ``B`` can
   be rewritten as:

   .. code-block::

      (defcell b (if (< a 10) a none))

If :cl:function:`NONE` is called while computing the initial value of
the cell, the cell is initialized to the value provided in the
optional argument to :cl:function:`NONE`, which defaults to ``NIL`` if
no argument is given.

The following definition of ``B`` ensures that it's value is
initialized to ``10``, rather than ``NIL``, if the initial value of
``A`` is greater than ``10``.

.. code-block::

   (defcell b
     (if (< a 10)
         a
	 (none 10)))

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
evaluates to :cl:symbol:`NIL` if ``N`` is less than ``0`` or an
integer could not be parsed from the string held in ``TEXT``, that is
a ``PARSE-ERROR`` condition was signaled while computing the value of
``N``.

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
