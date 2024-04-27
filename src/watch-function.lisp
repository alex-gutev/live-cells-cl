;;;; Provides functionality for defining a specification and generating
;;;; the implementation of a cell watch function.

(in-package :live-cells)


;;; Interface

(defclass watch-function-spec ()
  ((name
    :initarg :name
    :initform (gensym)
    :accessor name
    :documentation
    "Watch function identifier")

   (body
    :initarg :body
    :accessor body-forms
    :documentation
    "List of forms comprising the body of the watch function."))

  (:documentation
   "Defines the specification of a cell watch function."))

(defgeneric generate-watch-function (spec)
  (:documentation
   "Generate the implementation of a watch function defined by SPEC."))


;;; Implementation

(defmethod generate-watch-function ((spec watch-function-spec))
  (with-gensyms (updating?
                 waiting-for-change?
                 will-update
                 update
                 watch
                 arguments
                 arg
                 did-change)

    (with-slots (name body) spec
      `(let ((,updating? nil)
             (,waiting-for-change? nil)
             (,arguments (make-hash-set)))
         (labels ((,will-update (,arg)
                    (declare (ignore ,arg))
                    (when (not ,updating?)
                      (setf ,updating? t
                            ,waiting-for-change? nil)))

                  (,update (,arg ,did-change)
                    (declare (ignore ,arg))
                    (when (or ,updating? (and ,did-change ,waiting-for-change?))
                      (setf ,updating? nil
                            ,waiting-for-change? (not ,did-change))

                      (when ,did-change
                        (,watch))))

                  (,watch ()
                    (with-tracker
                        ((,arg)
                          (unless (memberp ,arg ,arguments)
                            (->> (nadjoin ,arg ,arguments)
                                 (setf ,arguments))

                            (call-add-observer
                             ,arg
                             (make-observer :key ',name
                                            :will-update #',will-update
                                            :update #',update))))
                      ,@body
                      )))
           (,watch))))))
