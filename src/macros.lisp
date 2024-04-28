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
