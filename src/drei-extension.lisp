(in-package :mcclim-panter)

(defvar *return-point* nil)

(clim:define-command (com-apropos-navigator
		      :name t 
		      :command-table drei:editing-table) ()
  (let ((*return-point* (drei:point)))
    (flexichain:insert-sequence *return-point*
				(write-to-string (run-apropos-navigator)))))
