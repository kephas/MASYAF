(in-package :thierry-technologies.com/2010/01/masyaf)

(defclass gamestate-with-array ()
  ((array :accessor gamestate-array :initarg :array)))

(defclass gamestate-with-rootset ()
  ((rootset :accessor gamestate-rootset :initarg :rootset)))

(defclass gamestate-with-origin ()
  ((origin :accessor gamestate-origin :initarg :origin)))

(defclass gamestate-with-adjacency ()
  ((adjacency :initarg :adjacency)))

(defmethod cells-adjacent-p (gamestate cell1 cell2)
  (member cell2 (gethash cell1 (slot-value gamestate 'adjacency))))

(defclass gamestate-with-information ()
  ((information-base :accessor gamestate-info-base :initarg :info)
   (new-information :accessor gamestate-new-info :initform nil)))
