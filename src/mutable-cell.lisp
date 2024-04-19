(in-package :live-cells)

(defclass mutable-cell-spec (cell-spec)
  ()

  (:documentation
   "Specification for a cell that can have its value set directly."))

(defmethod generate-extra ((spec mutable-cell-spec))
  (with-slots (value notify-will-update notify-update) spec
    (with-gensyms (new-value)
     `(progn
        (defsetf ,value () (,new-value)
          `(progn
             (,',notify-will-update)
             (setf ,',value ,,new-value)
             (,',notify-update)
             ,,new-value))))))
