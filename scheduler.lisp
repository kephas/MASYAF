(in-package :thierry-technologies.com/2010/01/masyaf)

(defun scheduler (gamestate agents add-pending extract-pending peek-pending)
  (with-functions (add-pending extract-pending peek-pending)
    (let ((agents-associations (make-hash-table)))
      (labels ((ensure-type (type)
		 (multiple-value-bind (list present-p) (gethash type agents-associations)
		   (declare (ignore list))
		   (unless present-p
		     (setf (gethash type agents-associations) nil)))))
	(dolist (agent agents)
	  (dolist (type (agent-symbols agent))
	    (ensure-type type)
	    (push agent (gethash type agents-associations))))
	(named-let loop-info ((info (extract-pending)))
	  (if info
	      (named-let loop-agent ((agents (gethash (information-symbol info) agents-associations)))
		(if agents
		    (let ((result (agent-apply (first agents) (gamestate-clone gamestate) info)))
		      (add-pending (gamestate-new-info result))
		      (gamestate-merge gamestate result)
		      (loop-agent (rest agents)))
		    (loop-info (extract-pending))))
	      gamestate))))))
