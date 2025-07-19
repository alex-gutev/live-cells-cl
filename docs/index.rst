.. live-cells-cl documentation master file, created by
   sphinx-quickstart on Tue Jul 15 07:40:24 2025.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

live-cells-cl documentation
===========================

Live-Cells-CL is a library that adds reactive programming to lisp. The
functionality of the library is ported from `Live Cells <https://livecell.gutev.dev/>`__ for Dart.

Examples
--------

Cells (reactive containers for data) are defined using
:cl:macro:`defcell`.

.. code-block::

   (defcell first-name "John")
   (defcell last-name "Smith")

Cells can be defined using arbitrary expressions that reference other
cells:

.. code-block::

   (defcell full-name
     (format nil "~a ~a" first-name last-name))
     
Cells are observed with :cl:macro:`live`:

.. code-block::

   (live
     (format t "Hello ~a~%" full-name))

Changing the value of a cell, using :cl:macro:`setf`, results in the
`live block` being run:

For example changing the values of ``FIRST-NAME`` and ``LAST-NAME``:

.. code-block::

   (setf first-name "Jane")
   (setf last-name "Doe")

results in the following being printed:

.. code-block:: text

   Hello Jane Smith
   Hello Jane Doe

For a more detailed explanation continue with this short tutorial.
   
.. toctree::
   :maxdepth: 2
   :caption: Contents:

   basics/cells.rst
   basics/cell-expressions.rst
   reference.rst

Indices and tables
==================

* :ref:`genindex`
* :ref:`search`
