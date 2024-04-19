;;;; Provides structures for representing Common Lisp function and
;;;; variable definitions, and functions for generating the actual
;;;; source code.

(in-package :live-cells)

(defstruct variable-spec
  "Specification of a variable definition.

NAME is the symbol identifying the variable.

INITFORM is the form computing the initial value of the variable.

TYPE is one of the following keywords identifying the type of
definition:

  -  :VARIABLE

     A variable defined with DEFVAR

  - :PARAM

     A variable defined with DEFPARAMETER

  - :SYMBOL-MACRO

     A symbol macro defined with DEFINE-SYMBOL-MACRO"

  name
  initform
  type)

(defstruct function-spec
  "Specification of a function definition.

NAME is the symbol naming the function.

LAMBDA-LIST is the function's lambda-list.

BODY is the list of forms comprising the function body.

TYPE is one of the following keywords identifying the type of
definition:

  - :FUNCTION

    A function defined with DEFUN

  - :MACRO

    A macro defined with DEFMACRO"

  name
  lambda-list
  body
  type)

(defgeneric generate-variable-definition (spec)
  (:documentation
   "Generate the CL code defining the variable specified by the
   `VARIABLE-SPEC' spec."))

(defgeneric generate-function-definition (spec)
  (:documentation
   "Generate the CL code defining the function specified by the
   `FUNCTION-SPEC' spec."))


;;; Implementation

(defmethod generate-variable-definition ((spec variable-spec))
  (with-accessors ((name variable-spec-name)
                   (initform variable-spec-initform)
                   (type variable-spec-type))
      spec

    (ecase type
      ((:variable :param)
       `(,(if (= type :variable)
              'defvar
              'defparameter)

         ,name
         ,initform))

      (:symbol-macro
       `(define-symbol-macro ,name ,initform)))))

(defmethod generate-function-definition ((spec function-spec))
  (with-accessors ((name function-spec-name)
                   (lambda-list function-spec-lambda-list)
                   (body function-spec-body)
                   (type function-spec-type))
      spec

    `(,(ecase type
         (:function 'defun)
         (:macro 'defmacro))

      ,name
      ,lambda-list
      ,@body)))
