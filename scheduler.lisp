(in-package :thierry-technologies.com/2010/01/masyaf)

(defun scheduler (gamestate agents add-pending extract-pending peek-pending)
  (with-functions (add-pending extract-pending peek-pending)
    (let ((agents-associations (make-hash-table)))
      (labels ((ensure-type (type)
		 (multiple-value-bind (list present-p) (gethash type agents-associations)
		   (declare (ignore list))
		   (unless present-p
		     (setf (gethash type agents-associations) nil)))))
	(map nil
	     (lambda (agent)
	       (map nil
		    (lambda (type)
		      (ensure-type type)
		      (push agent (gethash type agents-associations)))
		    (agent-symbols agent)))
	     agents)
	(named-let loop-info ()
	  (let ((info (extract-pending)))
	    (named-let loop-agent ((agents (gethash (information-symbol info) agents-associations)))
	      (when agents
		(let ((result (agent-apply (first agents) (gamestate-clone gamestate) info)))
		  (add-pending (gamestate-new-info result))
		  (gamestate-merge gamestate result)
		  (loop-agent (rest agents)))))
	    (if (or info (peek-pending))
		(loop-info)
		gamestate)))))))
