(in-package :thierry-technologies.com/2010/01/masyaf)

(defun origin-coordinates (dimensions)
  (if (zerop dimensions) nil (cons 0 (origin-coordinates (1- dimensions)))))

(defun next-point (coordinates maximums)
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
  (if from
      (restack (rest from) (cons (first from) to))
      to))

(defun neighbours (coordinates maximums)
  (labels ((rec (coordinates maximums previous neighbours)
	     (cons-bind (current rest coordinates)
	       (unless (zerop current)
		 (push (restack previous (cons (1- current) rest)) neighbours))
	       (unless (= current (first maximums))
		 (push (restack previous (cons (1+ current) rest)) neighbours))
	       (if rest 
		   (rec rest (rest maximums) (cons current previous) neighbours)
		   neighbours))))
    (rec coordinates maximums nil nil)))
