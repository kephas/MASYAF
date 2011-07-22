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

(defgeneric gamestate-renew-info (gamestate info))

(defmethod gamestate-renew-info ((gamestate gamestate-with-information) info)
  (push info (gamestate-new-info gamestate)))

(defgeneric gamestate-info-search (gamestate request)
  (:documentation "Convenience GF to search the IB of a gamestate."))

(defmethod gamestate-info-search ((gamestate gamestate-with-information) request)
  (information-search request (gamestate-info-base gamestate)))


; most games will work with a notion of a set of cells
(defgeneric gamestate-cell (gamestate designator))
(defgeneric retrieve-space-cell (container space designator))
(defgeneric store-space-cell (value container space designator))

(defmethod retrieve-space-cell ((container array) (space cartesian-space) (designator vector))
  (apply #'aref container (vect-coords designator)))

(defmethod store-space-cell (value (container array) (space cartesian-space) (designator vector))
  (setf (apply #'aref container (vect-coords designator)) value))


#| Gamestate with cells organized in a cartesian space (possibly with limits) |#

(defclass gamestate-with-space (spatial)
  ((cells :reader gamestate-cells :initform nil)))

(defmethod initialize-instance :after ((instance gamestate-with-space) &rest initargs &key filler &allow-other-keys)
  (declare (ignore initargs))
  (when filler
    (let ((space (space-of instance)))
      (with-slots (cells) instance
	(setf cells (make-array (space-dimensions space)))
	(do-grid (point space) ; TODO: maybe cartesian-specific
	  (store-space-cell (funcall filler point) cells space point))))))

(defmethod shared-clone :after ((object gamestate-with-space) (clone gamestate-with-space))
  (with-slots (cells) clone
    (setf cells (gamestate-cells object))))

(defmethod gamestate-cell ((gamestate gamestate-with-space) designator)
  (retrieve-space-cell (gamestate-cells gamestate) (space-of gamestate) designator))
