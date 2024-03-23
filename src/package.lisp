;;;; Packages providing the public interface of this library.

(defpackage :live-cells
  (:use
   :generic-cl
   :anaphora
   :arrows)

  (:import-from
   :alexandria
   :with-gensyms
   :format-symbol)

  (:import-from
   :agutil
   :defmacro!)

  (:documentation
   "Provides the API for using live cells."))

;;; Internal

(defpackage :live-cells-cell
  (:use)

  (:documentation
   "Package used for generated cell identifiers.

This package should NEVER be used by any other package."))
