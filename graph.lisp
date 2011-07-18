(in-package :thierry-technologies.com/2010/01/masyaf)

#| Objects contained in a space |#

(defclass spatial ()
  ((space :reader space :initarg :space)))

(defgeneric %in-space? (object space))
(defgeneric origin (object))
(defgeneric unit-vector (object))

(defun in-space? (object)
  (%in-space? object (space object)))


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

(defmethod origin ((object cartesian-space))
  (make-instance 'vector :space object :coords (make-sequence 'list (space-dimensions object) :initial-element 0)))

(defmethod unit-vector ((object cartesian-space))
  (make-instance 'vector :space object :coords (make-sequence 'list (space-dimensions object) :initial-element 1)))

(defmethod %in-space? ((object vector) (space cartesian-hyperoctant))
  (every #'< (vect-coords object) (space-size space)))

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

(defun next-point-in-grid (point &key (grid-unit (unit-vector (space point))))
  "When browsing through points in lexicographical order of their coordinates, returns the next point."
  (%next-point point (space point) grid-unit))

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
