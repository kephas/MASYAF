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
			     (gamestate-info-search game `(number _ ,@(vect-coords point)))))))

(defun existing-bridges (game island direction orientation)
  (length
   (gamestate-info-search game
			  `(bridge ,direction
				   ,@(vect-coords (funcall (make-movement direction orientation)
							   island))))))

(defun in-all-directions (game island fun)
  (mapcar (lambda (spec)
	    (cons-bind (direction orientation spec)
	      (funcall fun game island direction orientation)))
	  '((vertical . t)(vertical . nil)(horizontal . t)(horizontal . nil))))

(defun possible-directions (game island)
  (length (mapcan (lambda (n) (if (not (zerop n)) (list n)))
		  (in-all-directions game island #'possible-bridges))))

(defun sum (list)
  (reduce #'+ list))

(defun needed-bridges (game island)
  (- (island-number game island) (sum (in-all-directions game island #'existing-bridges))))

(defun is-island (game point)
  (gamestate-info-search game `(number _ ,@(vect-coords (vect-coords point)))))

(defun possible-bridges (game island direction orientation)
  (let* ((wall (turn direction))
	 (whole-path (make-path-to-island game island direction orientation))
	 (endpoint (first (last whole-path))))
    (if (is-island game endpoint)
	(named-let rec ((path (rest (butlast whole-path)))
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
	      (if room? (min possible (needed-bridges game endpoint)) 0)))
	0)))

(defun add-bridge (game island direction orientation)
  (let* ((whole-path (make-path-to-island game island direction orientation))
	 (endpoint (first (last whole-path))))
    (gamestate-renew-info game (first (gamestate-info-search game `(number _ ,@(vect-coords endpoint)))))
    (dolist (point (rest (butlast whole-path)) endpoint)
      (gamestate-add-info game `(bridge ,direction ,@(vect-coords point))))))


#| Agents |#

(defvar *saturate*
  (list '(number)
	(lambda (game info)
	  (let ((island (make-vector game (rest (information-arguments info)))))
	    (if (= (needed-bridges game island)
		   (sum (in-all-directions game island #'possible-bridges)))
		(in-all-directions game island
				   (lambda (game island dir orient)
				     (dotimes (i (possible-bridges game island dir orient))
				       (add-bridge game island dir orient))))))
	  game)))

(defvar *each-open-direction*
  (list '(number)
	(lambda (game info)
	  (let ((island (make-vector game (rest (information-arguments info)))))
	    (if (= (needed-bridges game island)
		   (1- (* 2 (possible-directions game island))))
		(in-all-directions game island
				   (lambda (game island dir orient)
				     (if (not (zerop (possible-bridges game island dir orient)))
					 (add-bridge game island dir orient))))))
	  game)))


#| Renderers |#

(defvar *bridges-renderers*
  (list (lambda (game coords)
	  (cif infos (gamestate-info-search game `(number _ ,@coords))
	       (aref (format nil "~a" (first (information-arguments (first infos)))) 0)))
	(lambda (game coords)
	  (cif infos (gamestate-info-search game `(bridge _ ,@coords))
	       (let ((chars '(horizontal "─═" vertical "│║")))
		 (elt (getf chars (first (information-arguments (first infos)))) (1- (length infos))))))
	(constantly #\space)))