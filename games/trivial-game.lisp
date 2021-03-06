(in-package :thierry-technologies.com/2010/01/masyaf)

(defun make-alternating-agents (color1 color2)
  (labels ((make-agent (c1 c2)
	     (list (list c1)
		   (lambda (game info)
		     (let ((point (make-instance 'vector :coords (information-arguments info) :space (space game))))
		       (dolist (coords (mapcar #'vect-coords (neighbours point 'manhattan-distance)) game)
			 (unless (gamestate-info-search game (list 'or (cons c2 coords) (cons c1 coords)))
			   (gamestate-add-info game (cons c2 coords)))))))))
    (list (make-agent color1 color2) (make-agent color2 color1))))

(defun render-bw-2d (game)
  (let ((size (space-size (space game))))
    (dotimes (i (first size))
      (dotimes (j (second size))
	(if (gamestate-info-search game (list 'black i j))
	    (if (gamestate-info-search game (list 'white i j))
		(princ "X")
		(princ "#"))
	    (princ " ")))
      (terpri))))

(defclass trivial-game (gamestate-with-information gamestate-with-space)())

(defun test-trivial-game (width height x y)
  (let ((game (make-instance 'trivial-game :info nil :space (make-instance 'cartesian-hyperoctant :size (list width height)))))
    (gamestate-add-info game (list 'black x y))
    (render-bw-2d (solve game (make-alternating-agents 'black 'white)))))
