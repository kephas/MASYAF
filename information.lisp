(in-package :thierry-technologies.com/2010/01/masyaf)

(defun simplify-dependencies (dependencies)
  "Returns a dependency clause simpler but equivalent to DEPENDENCIES.
The main AND clause will be flattened and any atomic predicate in this
flat clause will only appear once.
NOT WORKING: Any OR clause which contains an atomic predicate that is 
already present in the top AND clause will be discarded."
  (let ((and-clauses (make-hash-table))
	(or-clauses nil))
    (labels ((store-clauses (clause)
	       (if clause
		   (cons-bind (first rest clause)
		     (cond ((eq 'and first)
			    (store-clauses rest))
			   ((eq 'or first)
			    (push clause or-clauses))
			   (t (setf (gethash first and-clauses) t)
			      (store-clauses rest))))))
	     (unneeded-or (clause)
	       (if clause
		   (cons-bind (first rest clause)
		     (if (consp first)
			 (or (gethash first and-clauses) (unneeded-or rest))
			 (unneeded-or rest))))))
      (store-clauses dependencies)
      (append (cons 'and (let ((list))
			   (maphash (lambda (key value)
				      (declare (ignore value))
				      (push key list))
				    and-clauses)
			   list))
	      (remove-if #'unneeded-or or-clauses)))))
