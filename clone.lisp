(in-package :thierry-technologies.com/2010/01/masyaf)

#| Generic functions for object cloning |#

(defgeneric clone (object))

(defgeneric shared-clone (object clone)) ; users are expected to define :after methods on it


#| Base cases |#

(defmethod clone (object)
  (let ((clone (make-instance (class-of object))))
    (shared-clone object clone)
    clone))

(defmethod shared-clone (object clone))


#| Shallow cloning for some built-in classes |#

(defmethod clone ((object list))
  (copy-list object))

(defmethod clone ((object array))
  (copy-array object))
