(in-package :thierry-technologies.com/2010/01/masyaf)

(defun array->table (array)
  "Returns a GTK+ table displaying the given 2-dimensional array."
  (within-main-loop-and-wait 
    (let* ((dimensions (array-dimensions array))
	   (table (make-instance 'table :n-rows (first dimensions) :n-columns (second dimensions) :homogeneous t)))
      (named-let rec ((row 0)
		      (col 0))
	(if (= row (first dimensions))
	    table
	    (if (= col (second dimensions))
		(rec (1+ row) 0)
		(progn
		  (table-attach table (make-instance 'label :label (format nil "~a" (aref array row col)))
				col (1+ col) row (1+ row))
		  (rec row (1+ col)))))))))

(defun gtk-show-array (array)
  "Shows a new window displaying the given 2-dimensional array."
  (within-main-loop
    (let ((window (make-instance 'gtk-window)))
      (g-signal-connect window "delete-event"
			(lambda (window event)
			  (declare (ignore window event))
			  (gtk-main-quit)))
      (container-add window (array->table array))
      (widget-show window))))
