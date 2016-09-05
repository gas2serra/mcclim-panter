(in-package :mcclim-panter-apropos)

;;;
;;; navigator parameters
;;;

(defparameter *apropos-navigator-heading-text-style* (clim:make-text-style
						     nil
						     :bold 12))

(defparameter *apropos-navigator-subclas-of-options*
  (list (cons "nil" nil)
	(cons "sheet" 'clim:sheet)
	(cons "pane" 'clim:pane)
	(cons "gadget" 'clim:gadget)
	(cons "application-frame" 'clim:application-frame)))
