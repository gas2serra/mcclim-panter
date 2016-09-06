(in-package :mcclim-panter)

(defvar *return-point* nil)

(clim:define-command (com-apropos-navigator
		      :name t 
		      :command-table drei:editing-table) ()
  (let ((*return-point* (drei:point))
	(return-values (run-apropos-navigator)))
    (when return-values 
      (flexichain:insert-sequence *return-point*
				  (write-to-string return-values)))))

(esa-io::set-key 'com-apropos-navigator
		 'drei-lisp-syntax::lisp-table '((#\s :meta :control)))
