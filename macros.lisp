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
