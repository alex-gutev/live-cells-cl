(in-package :live-cells)

(defmacro defcell (name expression &environment env)
  (if (constantp expression env)
      `(define-mutable-cell% ,name ,expression)
      `(define-computed-cell% ,name ,expression)))

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
  (let ((spec (make-instance 'watch-function-spec :body forms)))
    (generate-watch-function spec)))

(defmacro cell-let ((&rest bindings) &body body &environment env)
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
