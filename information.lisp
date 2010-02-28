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

(defun every-apply (predicates &rest values)
  (if (apply (first predicates) values)
      (if (null (rest predicates))
	  t
	  (apply #'every-apply (rest predicates) values))
      nil))

(defun some-apply (predicates &rest values)
  (if (apply (first predicates) values)
      t
      (if (null (rest predicates))
	  nil
	  (apply #'every-apply (rest predicates) values))))

(defmethod information-symbol ((information list))
  (car information))

(defmethod information-arguments ((information list))
  (cdr information))

(defun clause-predicate (clause)
  (lambda (information)
    (if (eq (car clause) (information-symbol information))
	(every #'identity (mapcar (lambda (matcher value)
				    (or (eq matcher *joker*)
					(equal matcher value)))
				  (cdr clause)
				  (information-arguments information))))))

(defun request-predicate (request)
  (case (car request)
    ((not) (complement (request-predicate (cadr request))))
    ((and or)
     (lambda (information)
       (funcall (getf (list 'and #'every-apply 'or #'some-apply) (car request))
		(mapcar #'request-predicate (cdr request))
		information)))
    (t (clause-predicate request))))

(defmethod information-search (request (information-base list))
  (remove-if (complement (request-predicate request)) information-base))
