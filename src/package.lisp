(defpackage :live-cells.base
  (:use
   :generic-cl
   :anaphora
   :arrows
   :agutil)

  (:import-from
   :agutil
   :defmacro!)

  (:documentation
   "Provides the base constructs implementing the cell functionality.

This package should be used if new cell types are being defined, or
the core of live-cells is being extended."))

(defpackage :live-cells
  (:use
   :generic-cl
   :anaphora
   :arrows)

  (:documentation
   "Provides the API for using live cells."))

;;; Internal

(defpackage :live-cells-cell
  (:use)

  (:documentation
   "Package used for generated cell identifiers.

This package should NEVER be used by any other package."))
