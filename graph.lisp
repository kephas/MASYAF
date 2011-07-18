(in-package :thierry-technologies.com/2010/01/masyaf)

#| Objects contained in a space |#

(defclass spatial ()
  ((space :reader space :initarg :space)))

(defgeneric %in-space? (object space))
(defgeneric origin (object))
(defgeneric unit-vector (object))

(defun in-space? (object)
  (%in-space? object (space object)))

(defmethod shared-clone :after ((object spatial) (clone spatial))
  (with-slots (space) clone
    (setf space (space object))))


#| Spaces |#

(defclass cartesian-space ()
  ((dimensions :reader space-dimensions :initarg :dim)))

(defclass cartesian-hyperoctant (cartesian-space)
  ((size :reader space-size :initarg :size :initform nil)))

(defmethod space-dimensions ((object cartesian-hyperoctant))
  (length (space-size object)))

(defmethod %in-space? (object (space cartesian-space))
  (declare (ignore object space))
  t)


#| Vector in a space (also used as-is for points |#

(defclass vector (spatial)
  ((coordinates :reader vect-coords :initarg :coords)))

; if only coordinates are needed, sequences can be used as vectors
(defmethod vect-coords ((object sequence))
  (map 'list #'identity object))


(defun numbers (number times)
  (make-sequence 'list times :initial-element number))

(defmethod origin ((object cartesian-space))
  (make-instance 'vector :space object :coords (numbers 0 (space-dimensions object))))

(defmethod unit-vector ((object cartesian-space))
  (make-instance 'vector :space object :coords (numbers 1 (space-dimensions object))))

(defmethod %in-space? ((object vector) (space cartesian-hyperoctant))
  (and (every #'< (vect-coords object) (space-size space))
       (every (lambda (n) (>= n 0)) (vect-coords object))))


(defgeneric multiply (vector factor))

(defmethod multiply ((vector vector) (factor number))
  (make-instance 'vector :space (space vector)
		 :coords (mapcar (lambda (n) (* factor n)) (vect-coords vector))))


; pretty-printing of vectors
(defgeneric %print-vector (vector space stream))

(defmethod %print-vector (vector (space cartesian-space) stream)
  (format stream "[cartesian] ~a" (vect-coords vector)))

(defmethod %print-vector (vector (space cartesian-hyperoctant) stream)
  (call-next-method)
  (format stream " <~a>" (space-size space)))

(defmethod print-object ((object vector) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (%print-vector object (space object) stream)))


#| Point enumeration |#

(defun next-point-in-grid (point &optional grid-unit)
  "When browsing through points in lexicographical order of their coordinates, returns the next point."
  (let ((grid-unit (if grid-unit grid-unit (unit-vector (space point)))))
    (%next-point point (space point) grid-unit)))

(defgeneric %next-point (point space grid-unit))

(defmethod %next-point (point (space cartesian-hyperoctant) grid-unit)
  (labels ((rec (coordinates maximums unit terminal-p)
	     (multiple-value-bind (following remainder) (if (rest coordinates)
							    (rec (rest coordinates) (rest maximums) (rest unit) nil))
	       (if (or (not following) remainder)
		   (let ((next (+ (first coordinates) (first unit))))
		     (if (>= next (first maximums))
		       (if terminal-p
			   nil
			   (values (cons 0 following) t))
		       (cons next following)))
		   (cons (first coordinates) following)))))
    (cif coords (rec (vect-coords point) (space-size space) (vect-coords grid-unit) t)
	 (make-instance 'vector :coords coords :space space))))

(defun complete-grid (space &optional grid-unit)
  (named-let rec ((point (origin space))
		  (grid nil))
    (if point
	(rec (next-point-in-grid point grid-unit) (cons point grid))
	grid)))

(defmacro do-grid ((var space &optional grid-unit) &body body)
  (once-only (space grid-unit)
    `(named-let rec ((,var (origin ,space)))
       (when ,var
	 ,@body
	 (rec (next-point-in-grid ,var ,grid-unit))))))


#| Translation |#

(defgeneric %translate (object space move))

(defun translate (object move)
  (%translate object (space object) move))

(defmethod %translate ((object vector) (space cartesian-space) move)
  (make-instance 'vector
		 :coords (apply #'mapcar #'+ (mapcar #'vect-coords (list object move)))
		 :space space))


#| Neighbours |#

(defgeneric %neighbours (point space connetivity)
  (:documentation "Returns all neighbours of a point."))
(defgeneric unit-circle (space norm))

(defun neighbours (point connectivity)
  (%neighbours point (space point) connectivity))

(defmethod %neighbours (point (space cartesian-space) connectivity)
  (remove-if (complement #'in-space?) (mapcar (lambda (p)
						(translate p point))
					      (unit-circle space connectivity))))

(defmethod unit-circle ((space cartesian-space) (norm (eql 'manhattan-distance)))
  (named-let rec ((before 0)
		  (after (1- (space-dimensions space)))
		  (odd? t)
		  (points nil))
    (if (< after 0)
	points
	(if odd?
	    (rec before after (not odd?) (cons (make-instance 'vector :space space
							      :coords (append (numbers 0 before) (list 1) (numbers 0 after)))
					       points))
	    (rec (1+ before) (1- after) (not odd?) (cons (make-instance 'vector :space space
									:coords (append (numbers 0 before) (list -1) (numbers 0 after)))
							 points))))))

(defmethod unit-circle ((space cartesian-space) (norm (eql 'chebyshev-distance)))
  (let ((dimensions (space-dimensions space)))
    (remove (numbers 0 dimensions)
	    (mapcar (lambda (p)
		      (translate p (multiply (unit-vector space) -1)))
		    (complete-grid (make-instance 'cartesian-hyperoctant :size (numbers 3 dimensions))))
	    :key #'vect-coords
	    :test #'equal)))
