(in-package :live-cells)

(defmacro defcell (name value-form &environment env)
  "Define a global cell identified by NAME.

This macro defines a cell identified by NAME with its value computed
by VALUE-FORM. The value of the cell can be referenced by the symbol
NAME following the DEFCELL form.

If VALUE-FORM is a constant, by CONSTANTP, a mutable cell is defined
which can have its value set directly using SETF or SETQ on the symbol
NAME.

If VALUE-FORM is not a constant a computed cell is created. A computed
cell may reference one or more argument cells in VALUE-FORM or a
function called during the evaluation of VALUE-FORM.  When the values
of any of the argument cells change, the value of the computed cell is
recomputed by evaluating VALUE-FORM again. The referenced argument
cells are determined automatically when the VALUE-FORM is evaluated.

This macro creates a cell definition that is visible globally to all
forms unless it is lexically shadowed by a definition with the same
NAME using CELL-LET. Note that despite being visible globally, the
cell definition is still lexically scoped and not dynamically scoped
like global variables defined with DEFVAR or DEFPARAMETER."

  (if (constantp value-form env)
      `(define-mutable-cell% ,name ,value-form)
      `(define-computed-cell% ,name ,value-form)))

(defmacro define-mutable-cell% (name expression)
  (let ((spec (make-instance
               'mutable-cell-spec
               :name name
               :init expression)))

    (generate-cell-definition spec)))

(defmacro define-computed-cell% (name expression)
  (let ((spec (make-instance
               'compute-changes-only-cell-spec
               :name name
               :init expression)))

    (generate-cell-definition spec)))

(defmacro live (&body forms)
  "Define a live block consisting of FORMS.

The FORMS are evaluated once when the LIVE form is first evaluated,
after which they are evaluated again whenever the values of the cells
referenced by them change. Cells may be referenced either directly in
FORMS or by a function called during the evaluation of FORMS.

Returns a function which when called, stops the live block. Once
stopped the live block will no longer be called when the values of the
referenced cells change."

  (let ((spec (make-instance 'watch-function-spec :body forms)))
    (generate-watch-function spec)))

(defmacro with-live-scope (&body body)
  "Stop all live blocks defined in BODY on exiting the form.

All live blocks, defined using LIVE, are stopped when exiting the
dynamic extent defined by this form.

Multiple WITH-LIVE-SCOPE forms can be nested, in which case each form
stops the live blocks defined immediately within it."

  (with-gensyms (stoppers forms global-live)
    `(let ((,stoppers nil))
       (macrolet ((,global-live (&body ,forms)
                    (macroexpand `(live ,@,forms)))

                  (live (&body ,forms)
                    `(push (,',global-live ,@,forms) ,',stoppers)))
         (unwind-protect (progn ,@body)
           (foreach #'funcall ,stoppers))))))

(defmacro cell-let ((&rest bindings) &body body &environment env)
  "Define cells that are visible only to the forms in BODY.

BINDINGS is a list of cell bindings to establish, similar to
LET*. Each item in BINDINGS is a list of the form (NAME VALUE-FORM),
where NAME is the symbol identifying the cell and VALUE-FORM is the
value form of the cell. The cell can then be referenced within the
forms in BODY by the symbol NAME.

The cells defined in BINDINGS lexcially shadow those defined in the
global environment, with DEFCELL, or by an enclosing CELL-LET
form. Like LET* and unlike LET, each cell definition may reference the
cells defined earlier in the same CELL-LET.

The forms in BODY are evaluated in an implicit PROGN. The value
returned by the last form is returned by the CELL-LET form."

  (flet ((make-cell-binding (binding form)
           (destructuring-bind (name &optional initform)
               (ensure-list binding)

             (if (constantp initform env)
                 `(mutable-cell-let% (,name ,initform) ,form)
                 `(computed-cell-let% (,name ,initform) ,form)))))

    (->> `(progn ,@body)
         (reduce #'make-cell-binding bindings :from-end t :initial-value))))

(defmacro mutable-cell-let% ((name expression) &body body)
  (let* ((*global-cell-definition* nil)
         (spec (make-instance
                'mutable-cell-spec
                :name name
                :init expression)))

    (make-local-cell-definition% spec body)))

(defmacro computed-cell-let% ((name expression) &body body)
  (let* ((*global-cell-definition* nil)
         (spec (make-instance
                'compute-changes-only-cell-spec
                :name name
                :init expression)))

    (make-local-cell-definition% spec body)))

(defun make-local-cell-definition% (spec body)
  (let ((variables (generate-cell-variables spec))
        (functions (generate-cell-functions spec)))

    (labels ((make-variable-binding (variable)
               (list (variable-spec-name variable)
                     (variable-spec-initform variable)))

             (make-function-binding (function)
               (list* (function-spec-name function)
                      (function-spec-lambda-list function)
                      `(declare ,@(function-spec-declare function))
                      (function-spec-body function))))

      (let ((variables
              (-> (rcurry #'memberp '(:variable :param))
                  (remove-if-not variables :key #'variable-spec-type)))

            (symbol-macros
              (-> (curry #'= :symbol-macro)
                  (remove-if-not variables :key #'variable-spec-type)))

            (functions
              (-> (rcurry #'= :function)
                  (remove-if-not functions :key #'function-spec-type)))

            (macros
              (-> (rcurry #'= :macro)
                  (remove-if-not functions :key #'function-spec-type))))

        `(symbol-macrolet ,(map #'make-variable-binding symbol-macros)
           (let ,(map #'make-variable-binding variables)
             (macrolet ,(map #'make-function-binding macros)
               (labels ,(map #'make-function-binding functions)
                 ,@body))))))))
