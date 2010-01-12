(in-package :thierry-technologies.com/2010/01/nurikabe)

(defmacro cons-bind ((car-var cdr-var cons-cell) &body body)
  (once-only (cons-cell)
    `(let ((,car-var (car ,cons-cell))
	   (,cdr-var (cdr ,cons-cell)))
       ,@body)))
