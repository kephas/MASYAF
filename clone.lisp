(in-package :thierry-technologies.com/2010/01/masyaf)

(defgeneric clone (object))

(defgeneric shared-clone (object clone))


(defmethod clone (object)
  (let ((clone (make-instance (class-of object))))
    (shared-clone object clone)
    clone))

(defmethod shared-clone (object clone))
