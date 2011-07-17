(in-package :thierry-technologies.com/2010/01/masyaf)

(defun scheduler (gamestate agents add-pending extract-pending)
  (with-functions (add-pending extract-pending)
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
	(named-let loop-info ((gamestate gamestate)
			      (info (extract-pending)))
	  (if info
	      (named-let loop-agent ((gamestate gamestate)
				     (agents (gethash (information-symbol info) agents-associations)))
		(if agents
		    (let ((result (agent-apply (first agents) (clone gamestate) info)))
		      (add-pending (gamestate-new-info result))
		      (loop-agent result (rest agents)))
		    (loop-info gamestate (extract-pending))))
	      gamestate))))))

(defun solve (gamestate agents)
  (let ((pending (gamestate-new-info gamestate)))
    (scheduler gamestate agents
	       (lambda (new-infos)
		 (setf pending (append new-infos pending)))
	       (lambda ()
		 (prog1 (first pending)
		   (setf pending (rest pending)))))))
