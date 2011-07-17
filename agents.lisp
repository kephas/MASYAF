(in-package :thierry-technologies.com/2010/01/masyaf)

(defgeneric agent-symbols (agent)
  (:documentation "Returns the list of symbols that this agents can react to."))

(defgeneric agent-apply (agent gamestate information)
  (:documentation "Makes AGENT react to information INFORMATION in the game GAMESTATE."))


(defmethod agent-symbols ((agent list))
  (car agent))

(defmethod agent-apply ((agent list) gamestate information)
  (funcall (cadr agent) gamestate information))