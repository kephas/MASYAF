(defpackage :thierry-technologies.com/2010/01/masyaf/trivial
  (:use :cl :thierry-technologies.com/2010/01/masyaf)
  (:export
   #:test-trivial-game))
   
(in-package :thierry-technologies.com/2010/01/masyaf/trivial)

(defun make-alternating-agents (color1 color2)
  (labels ((make-agent (c1 c2)
	     (list (list c1)
		   (lambda (game info)
		     (labels ((clause (color coords)
				`(color ,color ,@coords)))
		       (let ((point (make-instance 'sp-vector :coords (information-arguments info) :space (space-of game))))
			 (dolist (coords (mapcar #'vect-coords (neighbours point 'manhattan-distance)) game)
			   (unless (gamestate-info-search game `(color _ coords))
			     (gamestate-add-info game (clause c2 coords))))))))))
    (list (make-agent color1 color2) (make-agent color2 color1))))

(defclass trivial-game (gamestate-with-information gamestate-with-space)())

(defvar *char-renderers*
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
				    *char-renderers*)))
