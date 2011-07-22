(in-package :thierry-technologies.com/2010/01/masyaf)

(defun make-alternating-agents (color1 color2)
  (labels ((make-agent (c1 c2)
	     (list (list c1)
		   (lambda (game info)
		     (let ((point (make-instance 'sp-vector :coords (rest (information-arguments info)) :space (space-of game))))
		       (dolist (coords (mapcar #'vect-coords (remove-if (complement #'in-space?) (neighbours point 'manhattan-distance))) game)
			 (unless (gamestate-info-search game `(color _ ,coords))
			   (gamestate-add-info game `(color ,c2 ,coords)))))))))
    (list (make-agent color1 color2) (make-agent color2 color1))))

(defclass trivial-game (gamestate-with-information gamestate-with-space)())

(defvar *trivial-game-char-renderers*
  (list (lambda (game coords)
	  (cif infos (gamestate-info-search game `(color _ ,@coords))
	       (if (> (length infos) 1)
		   #\X ; there shouldn't be more than one color per cell
		   (case (first (information-arguments (first infos)))
		     ((white) #\space)
		     ((black) #\#)))))))

(defun test-trivial-game (width height x y)
  (let ((game (make-instance 'trivial-game :info nil :space (make-instance 'cartesian-hyperoctant :size (list width height)))))
    (gamestate-add-info game `(color black ,x ,y))
    (render-2d-spatial-game-by-char (solve game (make-alternating-agents 'black 'white))
				    *trivial-game-char-renderers*)))
