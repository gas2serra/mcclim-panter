(in-package :mcclim-panter-apropos)

;;;
;;; navigator parameters
;;;

(defparameter *apropos-navigator-heading-text-style* (clim:make-text-style
						     nil
						     :bold 13))

(defparameter *apropos-navigator-sub-heading-text-style* (clim:make-text-style
						     nil
						     :bold 11))

(defparameter *apropos-navigator-subclas-of-options*
  (list (cons "nil" nil)
	(cons "sheet" 'clim:sheet)
	(cons "pane" 'clim:pane)
	(cons "gadget" 'clim:gadget)
	(cons "presentation" 'clim:presentation)
	(cons "view" 'clim:view)
	(cons "command-table" 'clim:command-table)
	(cons "application-frame" 'clim:application-frame)))

(defparameter *apropos-navigator-metaclas-of-options*
  (list (cons "nil" nil)
	(cons "presentation-type" 'climi::presentation-type-class)))
	
(defparameter *apropos-navigator-preselect-options*
  (list (cons "nil" nil)
	(cons "commands"
	      #'command-internals-symbol-p)))



