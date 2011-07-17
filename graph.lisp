(in-package :thierry-technologies.com/2010/01/masyaf)

(defclass cartesian-point ()
  ((coordinates :reader point-coords :initarg :coords)
   (space-size :reader point-max :initarg :max :documentation "coordinates of the corner of the available space farthest from origin")))

(defun origin-coordinates (dimensions)
  (if (zerop dimensions) nil (cons 0 (origin-coordinates (1- dimensions)))))

(defgeneric next-point (point)
  (:documentation "When browsing through points in order of their coordinates, returns the next point."))

(defmethod next-point ((point cartesian-point))
  (labels ((rec (coordinates maximums terminal-p)
	     (multiple-value-bind (following remainder) (if (rest coordinates)
							    (rec (rest coordinates) (rest maximums) nil))
	       (if (or (not following) remainder)
		   (if (= (first coordinates) (first maximums))
		       (if terminal-p
			   nil
			   (values (cons 0 following) t))
		       (cons (1+ (first coordinates)) following))
		   (cons (first coordinates) following)))))
    (rec coordinates maximums t)))

(defun restack (from to)
  "Tail-recursive (append (reverse FROM) TO)"
  (if from
      (restack (rest from) (cons (first from) to))
      to))

(defgeneric neighbours (point)
  (:documentation "Returns all neighbours of a point."))

(defmethod neighbours ((point cartesian-point))
  (labels ((rec (coordinates maximums previous neighbours)
	     (cons-bind (current rest coordinates)
	       (unless (zerop current)
		 (push (restack previous (cons (1- current) rest)) neighbours))
	       (unless (= current (first maximums))
		 (push (restack previous (cons (1+ current) rest)) neighbours))
	       (if rest 
		   (rec rest (rest maximums) (cons current previous) neighbours)
		   neighbours))))
    (rec (point-coords point) (point-max point) nil nil)))
