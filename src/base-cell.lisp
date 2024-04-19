;;;; Provides the base interface for cell code generation.

(in-package :live-cells)


;;; Cell specification

(defconstant +cell-def-package+ (find-package "LIVE-CELLS-CELL")
  "The package in symbols identifying generated cell definitions are
interned.")

(defun cell-symbol (name suffix)
  "Generate a new symbol for a cell definition.

The format of the generated symbol is NAME-SUFFIX followed by random
identifier. The symbol is interned in +CELL-DEF-PACKAGE+."

  (-> (format-symbol nil "~a-~a" name suffix)
      string
      (gentemp +cell-def-package+)))

(defclass cell-spec ()
  ((name
    :initarg :name
    :accessor name
    :documentation
    "Cell identifier")

   (value
    :initarg :value
    :initform (cell-symbol 'cell 'cell-value)
    :accessor value
    :documentation
    "Name of the variable holding the cell's value.")

   (init-form
    :initarg :init
    :accessor init-form
    :documentation
    "Form that computes the cell's value")

   (observers
    :initarg :observers
    :initform (cell-symbol 'cell 'cell-observers)
    :accessor observers
    :documentation
    "Name of the variable holding the cell's observer set.")

   (add-observer
    :initarg :add-observer
    :initform (cell-symbol 'cell 'add-observer)
    :accessor add-observer
    :documentation
    "Name of the function for adding an observer to this cell.")

   (remove-observer
    :initarg :remove-observer
    :initform (cell-symbol 'cell 'remove-observer)
    :accessor remove-observer
    :documentation
    "Name of the function for removing a cell observer.")

   (notify-update
    :initarg :notify-update
    :initform (cell-symbol 'cell 'notify-update)
    :accessor notify-update
    :documentation
    "Name of the function for notifying the observers of this cell
    that its value has changed.")

   (notify-will-update
    :initarg :notify-will-update
    :initform (cell-symbol 'cell 'notify-will-update)
    :accessor notify-will-update
    :documentation
    "Name of the function for notifying the observers of this cell
    that its value will change."))

  (:documentation
   "Specification for a cell, that is used by code generators to
   generate the actual implementation."))


;;; Code generation interface

(defgeneric generate-cell-definition (spec)
  (:documentation
   "Generate the form implementing the cell defined by SPEC."))

(defgeneric generate-cell-variables (spec)
  (:documentation
   "Generate the list of variables for the cell defined by SPEC.

Returns a list of `VARIABLE-SPEC's."))

(defgeneric generate-cell-functions (spec)
  (:documentation
   "Generate the list of functions for the cell defined by SPEC.

Returns a list of `FUNCTION-SPEC's."))

(defgeneric generate-use-cell (spec)
  (:documentation
   "Generate a form that references the value of the cell defined by
   SPEC, and track it as an argument."))

(defgeneric generate-argument-record (spec)
  (:documentation
   "Generate the `ARGUMENT' record for the cell defined by SPEC."))

(defgeneric generate-add-observer (spec arg)
  (:documentation
   "Generate the function for adding an observer to the cell defined by SPEC.

ARG is the name of the symbol to use for the function argument.

The return value should be a single form defining only the body of the
function."))

(defgeneric generate-remove-observer (spec arg)
  (:documentation
   "Generate the function for removing an observer from the cell defined by SPEC.

ARG is the name of the symbol to use for the function argument.

The return value should be a single form defining only the body of the
function."))

(defgeneric generate-notify-update (spec)
  (:documentation
   "Generate the function for notifying the observers of the cell defined by SPEC, that its value has changed.

The return value should be a single form defining only the body of the
function."))

(defgeneric generate-notify-will-update (spec)
  (:documentation
   "Generate the function for notifying the observers of the cell defined by SPEC, that its value will change.

The return value should be a single form defining only the body of the
function."))

(defgeneric generate-extra (spec)
  (:documentation
   "Generate any extra definitions required by the cell defined by SPEC.

The return value should be a single form which is inserted after the
remaining definitions."))

(defgeneric generate-init (spec)
  (:documentation
   "Generate the initialization function of the cell defined by SPEC.

The initialization function is called before adding the first observer
on the cell.

The return value should be a single form defining only the body of the
function. If NIL is returned, no init function is generated."))

(defgeneric generate-pause (spec)
  (:documentation
   "Generate the cleanup function of the cell defined by SPEC.

The cleanup function is called after removing the last observer.

The return value should be a single form defining only the body of the
function. If NIL is returned, no cleanup function is generated."))


;;; Implementations

(defmethod generate-argument-record ((spec cell-spec))
  (with-slots (name add-observer remove-observer) spec
    `(make-argument
      :key ',name
      :add-observer #',add-observer
      :remove-observer #',remove-observer)))

(defmethod generate-add-observer ((spec cell-spec) arg)
  (with-slots (observers) spec
    `(progn
       ,@(awhen (generate-init spec)
           `((when (emptyp ,observers)
               ,it)))

       (incf (get ,arg ,observers 0)))))

(defmethod generate-remove-observer ((spec cell-spec) arg)
  (with-slots (observers) spec
    `(progn
       (awhen (get ,arg ,observers)
         (if (= it 1)
             (erase ,observers ,arg)
             (decf (get ,arg ,observers))))

       ,@(awhen (generate-pause spec)
           `((when (emptyp ,observers)
               ,it))))))

(defmethod generate-init ((spec cell-spec)) nil)
(defmethod generate-pause ((spec cell-spec)) nil)
(defmethod generate-extra ((spec cell-spec)) nil)

(defmethod generate-use-cell ((spec cell-spec))
  (with-slots (value) spec
    `(progn
       `(progn
          ,'(track-argument ,(generate-argument-record spec))
          ,',value))))

(defmethod generate-notify-will-update ((spec cell-spec))
  (with-slots (observers) spec
    (with-gensyms (observer)
      `(progn
         (doseq (,observer (map-keys ,observers))
           (call-will-update ,observer ,(generate-argument-record spec)))))))

(defmethod generate-notify-update ((spec cell-spec))
  (with-slots (observers) spec
    (with-gensyms (observer)
      `(progn
         (doseq (,observer (map-keys ,observers))
           (call-update ,observer ,(generate-argument-record spec)))))))

(defmethod generate-cell-variables ((spec cell-spec))
  (with-slots (name
               value
               init-form
               observers)
      spec

    (list
     (make-variable-spec
      :name name
      :initform `(,value)
      :type :symbol-macro)

     (make-variable-spec
      :name value
      :initform init-form
      :type :param)

     (make-variable-spec
      :name observers
      :initform '(make-hash-map)
      :type :variable))))

(defmethod generate-cell-functions ((spec cell-spec))
  (with-slots (name
               value
               init-form
               observers
               add-observer
               remove-observer
               notify-update
               notify-will-update)
      spec

    (with-gensyms (observer)
      (list

       (make-function-spec
        :name add-observer
        :type :function
        :lambda-list `(,observer)
        :body (list (generate-add-observer spec observer)))

       (make-function-spec
        :name remove-observer
        :type :function
        :lambda-list `(,observer)
        :body (list (generate-remove-observer spec observer)))

       (make-function-spec
        :name notify-update
        :type :function
        :lambda-list ()
        :body (list (generate-notify-update spec)))

       (make-function-spec
        :name notify-will-update
        :type :function
        :lambda-list ()
        :body (list (generate-notify-will-update spec)))

       (make-function-spec
        :name value
        :type :macro
        :lambda-list ()
        :body (list (generate-use-cell spec)))))))

(defmethod generate-cell-definition ((spec cell-spec))
  `(progn
     ,@(->> (generate-cell-variables spec)
            (map #'generate-variable-definition))

     ,@(->> (generate-cell-functions spec)
            (map #'generate-function-definition))

     ,(generate-extra spec)))
