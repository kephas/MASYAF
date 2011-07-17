(in-package :thierry-technologies.com/2010/01/masyaf)

#| Base class of MASYAF gamestate |#

(defclass gamestate-with-information ()
  ((information-base :accessor gamestate-info-base :initarg :info)
   (new-information :accessor gamestate-new-info :initform nil)))

(defmethod shared-clone :after ((object gamestate-with-information) (clone gamestate-with-information))
  (setf (gamestate-info-base clone) (clone (gamestate-info-base object))))

(defgeneric gamestate-add-info (gamestate info))

(defmethod gamestate-add-info ((gamestate gamestate-with-information) info)
  (setf (gamestate-info-base gamestate) (information-add info (gamestate-info-base gamestate)))
  (push info (gamestate-new-info gamestate)))

(defgeneric gamestate-info-search (gamestate request)
  (:documentation "Convenience GF to search the IB of a gamestate."))

(defmethod gamestate-info-search ((gamestate gamestate-with-information) request)
  (information-search request (gamestate-info-base gamestate)))


; most games will work with a notion of a set of cells
(defgeneric gamestate-cell (gamestate designator))


#| Gamestate with cells organized in a cartesian space with limits |#

(defclass gamestate-with-cartesian-space ()
  ((size :accessor gamestate-space-size :initarg :size)
   (space :accessor gamestate-space :initform nil)))

(defmethod initialize-instance :after ((instance gamestate-with-cartesian-space) &rest initargs &key filler &allow-other-keys)
  (declare (ignore initargs))
  (when filler
    (let* ((size (gamestate-space-size instance))
	   (space (make-array size :initial-element nil)))
      (named-let rec ((point (make-instance 'cartesian-point :coords (origin-coordinates (length size)) :max size)))
	(when point
	  (setf (apply #'aref space (point-coords point)) (funcall filler point))
	  (rec (next-point point))))
      (setf (gamestate-space instance) space))))

(defmethod shared-clone :after ((object gamestate-with-cartesian-space) (clone gamestate-with-cartesian-space))
  (setf (gamestate-space clone) (clone (gamestate-space object))
	(gamestate-space-size clone) (gamestate-space-size object)))

(defmethod gamestate-cell ((gamestate gamestate-with-cartesian-space) (designator cartesian-point))
  (apply #'aref (gamestate-space gamestate) (point-coords designator)))
