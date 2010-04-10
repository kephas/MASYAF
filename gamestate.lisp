(in-package :thierry-technologies.com/2010/01/masyaf)

(defclass gamestate-with-information ()
  ((information-base :accessor gamestate-info-base :initarg :info)
   (new-information :accessor gamestate-new-info :initform nil)
   (premises :accessor gamestate-premises :initform nil)))

(defmethod gamestate-add-premise (gamestate premise)
  (push premise (gamestate-premises gamestate)))

(defclass information-monitor ()
  ((gamestate :accessor monitor-gamestate :initarg :gamestate)
   (informations :accessor monitor-infos :initarg :infos)))

(defmethod next-result (monitor)
  (cons-bind (next rest (monitor-infos monitor))
    (setf (monitor-infos monitor) rest)
    (gamestate-add-premise (monitor-gamestate monitor) next)
    next))

(defmethod gamestate-search (gamestate request)
  (make-instance 'information-monitor
		 :gamestate gamestate
		 :infos (information-search request (gamestate-info-base gamestate))))

(defclass gamestate-with-array (gamestate-with-information)
  ((array :accessor gamestate-array :initarg :array)))

(defclass gamestate-with-rootset (gamestate-with-information)
  ((rootset :accessor gamestate-rootset :initarg :rootset)))

(defclass gamestate-with-origin (gamestate-with-information)
  ((origin :accessor gamestate-origin :initarg :origin)))

(defclass gamestate-with-adjacency (gamestate-with-information)
  ((adjacency :initarg :adjacency)))

(defmethod cells-adjacent-p (gamestate cell1 cell2)
  (member cell2 (gethash cell1 (slot-value gamestate 'adjacency))))

