(in-package :thierry-technologies.com/2010/01/masyaf)

(defgeneric information-p (object))

(defmethod information-p ((object list))
  (let ((first (first object)))
    (and (symbolp first) (not (find first '(and or))))))

(defun simplify-dependencies (dependencies)
  (declare (optimize (debug 3)))
  "Returns a dependency clause simpler but equivalent to DEPENDENCIES.
The main AND clause will be flattened and any atomic predicate in this
flat clause will only appear once.
OR clauses must be already flat. They will be removed if unnecessary."
  (let ((and-clauses (make-hash-table))
	(or-clauses nil))
    (labels ((store-clauses (clause next)
	       (if clause
		   (case (first clause)
		     ((and) (store-and-clauses (rest clause) next))
		     ((or) (store-or-clause (rest clause) next)))
		   (when next
		     (store-clauses (first next) (rest next)))))
	     (store-and-clauses (clauses next)
	       (if clauses
		   (cons-bind (first rest clauses)
		     (if (information-p first)
			 (progn 
			   (setf (gethash first and-clauses) t)
			   (store-and-clauses rest next))
			 (store-clauses first (cons (cons 'and rest) next))))
		   (store-clauses nil next)))
	     (store-or-clause (clause next)
	       (let ((canonical (cons 'or (sort clause #'< :key #'sxhash))))
		 (unless (find canonical or-clauses :test #'equal)
		   (push canonical or-clauses))
		 (store-clauses nil next)))
	     (unneeded-or (clause)
	       (if clause
		   (cons-bind (first rest clause)
		     (or (gethash first and-clauses) (unneeded-or rest)))
		   nil)))
      (store-clauses dependencies nil)
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

(defgeneric information-symbol (information))
(defgeneric information-arguments (information))

(defmethod information-symbol ((information list))
  (car information))

(defmethod information-arguments ((information list))
  (cdr information))

(defvar *joker* '_)

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

(defgeneric information-search (request information-base))
(defgeneric information-add (new information-base))

(defmethod information-search (request (information-base list))
  (remove-if (complement (request-predicate request)) information-base))

(defmethod information-add (new (information-base list))
  (cons new information-base))
