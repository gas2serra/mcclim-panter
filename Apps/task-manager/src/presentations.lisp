(in-package :mcclim-panter-task-manager)

(clim:define-presentation-method clim:present
    (object (type clim:application-frame) stream
	    (view clim:textual-view)
	    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (prin1 object stream))

(clim:define-presentation-type thread ())

(clim:define-presentation-method clim:present
    (object (type thread) stream
	    (view clim:textual-view)
	    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (prin1 object stream))
