;;;; Provides functionality for generating the definition of a cell
;;;; that is a function of one or more argument cells.

(in-package :live-cells)


;;; Interface

(defclass compute-cell-spec (observer-cell-spec)
  ((arguments
    :initarg :arguments
    :initform (cell-symbol 'cell 'arguments)
    :accessor arguments
    :documentation
    "Name of the variable holding the cell's argument set.")

   (compute
    :initarg :compute
    :initform (cell-symbol 'cell 'compute)
    :accessor compute
    :documentation
    "Name of the compute value function for this cell."))

  (:documentation
   "Specification for a cell of which the value is a function of one
   or more argument cells."))

(defclass compute-changes-only-cell-spec (compute-cell-spec changes-only-cell-spec)
  ()

  (:documentation
   "Specification for a computed cell that only notifies its observers
   when its new value is /= to its previous value."))

(defgeneric generate-observer-record (spec)
  (:documentation
   "Generate the `OBSERVER' record for the cell defined by SPEC."))

(defgeneric generate-compute (spec)
  (:documentation
   "Generate the compute value function for the cell defined by SPEC.

The return value of this function should be a single form that
computes the value of the cell."))


;;; Implementation

(defmethod generate-init ((spec compute-cell-spec))
  "Generate code which adds the cell as an observer of its arguments."

  (with-slots (arguments) spec
    (with-gensyms (arg)
      `(progn
         (doseq (,arg ,arguments)
           (call-add-observer ,arg ,(generate-observer-record spec)))))))

(defmethod generate-pause ((spec compute-cell-spec))
  "Generate code which stops observing the argument cells."

  (with-slots (arguments) spec
    (with-gensyms (arg)
      `(progn
         (doseq (,arg ,arguments)
           (call-remove-observer ,arg ,(generate-observer-record spec)))))))

(defmethod generate-use-cell ((spec compute-cell-spec))
  (with-gensyms (condition)
    (with-slots (stale? has-value? value compute) spec
      `(progn
         `(progn
            ,'(track-argument ,(generate-argument-record spec))

            (when ,',stale?
              (handler-case
                  (setf ,',value (,',compute))

                (stop-computation (,',condition)
                  (unless ,',has-value?
                    (setf ,',value
                          (default-value ,',condition)))))

              (setf ,',stale? nil)
              (setf ,',has-value? t))

            ,',value)))))

(defmethod generate-compute ((spec compute-cell-spec))
  (with-slots (init-form arguments) spec
    (with-gensyms (arg)
      `(with-tracker
           ((,arg)
             (when (not (memberp ,arg ,arguments))
               (call-add-observer ,arg ,(generate-observer-record spec))
               (->> (nadjoin ,arg ,arguments)
                    (setf ,arguments))))

         ,init-form))))

(defmethod generate-observer-record ((spec compute-cell-spec))
  (with-slots (name will-update update) spec
    `(make-observer
      :key ',name
      :will-update #',will-update
      :update #',update)))

(defmethod generate-cell-variables ((spec compute-cell-spec))
  (with-slots (arguments) spec
    (list*
     (make-variable-spec
      :name arguments
      :initform '(make-hash-set)
      :type :variable)

     (let ((*bind-init-form-p* nil))
       (call-next-method)))))

(defmethod generate-cell-functions ((spec compute-cell-spec))
  (with-slots (compute) spec
    (append
     (call-next-method)

     (list
      (make-function-spec
       :name compute
       :type :function
       :lambda-list ()
       :body (list (generate-compute spec)))))))
