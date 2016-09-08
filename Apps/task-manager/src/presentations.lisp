(in-package :mcclim-panter-task-manager)

(clim:define-presentation-method clim:present
    (object (type clim:application-frame) stream
	    (view clim:textual-view)
	    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "~A (~A) [~A]"
	  (clim:frame-pretty-name object)
	  (clim:frame-name object)
	  (clim:transform-region (clim:sheet-transformation
				  (clim:frame-top-level-sheet object))
				 (clim:sheet-region
				  (clim:frame-top-level-sheet object)))))
