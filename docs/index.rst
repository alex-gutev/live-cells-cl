.. live-cells-cl documentation master file, created by
   sphinx-quickstart on Tue Jul 15 07:40:24 2025.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

live-cells-cl documentation
===========================

Live-Cells-CL is a library that adds reactive programming to lisp. The
functionality of the library is ported from `Live Cells <https://livecell.gutev.dev/>`__ for Dart.

The source code for this library is available on `Github`_.

.. _Github: https://github.com/alex-gutev/live-cells-cl

.. note::

   This library, and in its name in particular, was inspired by
   `Cells`_.

   .. _Cells: https://github.com/kennytilton/cells

Examples
--------

.. attention::

   This library is still in beta and may experience breaking changes
   between releases.

Cells (reactive containers for data) are defined using
:cl:macro:`DEFCELL`.

.. code-block::

   (defcell first-name "John")
   (defcell last-name "Smith")

Cells can be defined using arbitrary expressions that reference other
cells:

.. code-block::

   (defcell full-name
     (format nil "~a ~a" first-name last-name))
     
Cells are observed with :cl:macro:`LIVE`:

.. code-block::

   (live
     (format t "Hello ~a~%" full-name))

Changing the value of a cell, using :cl:macro:`COMMON-LISP:SETF`, results in the
`live block` being run:

For example changing the values of ``FIRST-NAME`` and ``LAST-NAME``:

.. code-block::

   (setf first-name "Jane")
   (setf last-name "Doe")

results in the following being printed:

.. code-block:: text

   Hello Jane Smith
   Hello Jane Doe


Documentation
=============
   
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
