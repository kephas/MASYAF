(in-package :thierry-technologies.com/2010/01/masyaf)

(defmacro cons-bind ((car-var cdr-var cons-cell) &body body)
  (once-only (cons-cell)
    `(let ((,car-var (car ,cons-cell))
	   (,cdr-var (cdr ,cons-cell)))
       ,@body)))

(defmacro let-conses ((&rest names) &body body)
  "Used to test SIMPLIFY-DEPENDENCIES"
  `(let ,(mapcar (lambda (name) `(,name (cons ',name nil))) names)
     ,@body))

(defmacro with-functions (names &body body)
  `(labels (,@(mapcar (lambda (name-spec)
			(if (consp name-spec)
			    `(,(car name-spec) (&rest rest) (apply ,(cadr name-spec) rest))
			    `(,name-spec (&rest rest) (apply ,name-spec rest))))
		      names))
     ,@body))

(defmacro named-let (name binds &body body)
  `(labels ((,name ,(mapcar #'first binds) ,@body))
     (,name ,@(mapcar #'second binds))))
