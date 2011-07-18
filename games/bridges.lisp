(in-package :thierry-technologies.com/2010/01/masyaf)

(defclass bridges-game (gamestate-with-information gamestate-with-space)())

(defun except-first (fun &optional value)
  (let ((first t))
    (lambda (&rest args)
      (if first
	  (progn
	    (setf first nil)
	    value)
	  (apply fun args)))))

(defun turn (direction)
  (case direction
    ((horizontal) 'vertical)
    ((vertical) 'horizontal)))

(defun make-movement (direction orientation)
  (make-translation (multiply (make-instance 'vector :space (make-instance 'cartesian-space :dim 2)
					     :coords (case direction
						       ((horizontal) '(1 0))
						       ((vertical) '(0 1))))
			      (if orientation 1 -1))))

(defun make-path-to-island (game island direction orientation)
  (make-path island (make-movement direction orientation)
	     (except-first (lambda (point)
			     (gamestate-info-search game (cons 'number (vect-coords point)))))))

(defun possible-bridges (game island direction orientation)
  (let ((wall (turn direction)))
    (named-let rec ((path (rest (make-path-to-island game island direction orientation)))
		    (possible 2)
		    (room? nil))
      (if path
	  (cons-bind (first rest path)
	    (if (zerop possible)
		0
		(rec rest (min possible
			       (if (gamestate-info-search game `(bridge ,wall ,@(vect-coords first))) 0 2)
			       (- 2 (length (gamestate-info-search game `(bridge ,direction ,@(vect-coords first))))))
		     t)))
	  (if room? possible 0)))))

(defun existing-bridges (game island)
  (reduce #'+ (mapcar (lambda (spec)
			(cons-bind (direction orientation spec)
			  (length
			   (gamestate-info-search game
						  `(bridge ,direction
							   ,@(vect-coords (funcall (make-movement direction orientation)
										   island)))))))
		      '((vertical . t)(vertical . nil)(horizontal . t)(horizontal . nil))))))

(defun add-bridge (game island direction orientation)
  (map nil (except-first (lambda (point)
			   (gamestate-add-info game `(bridge ,direction ,@(vect-coords point)))))
       (make-path-to-island game island direction orientation)))
