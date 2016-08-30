(in-package :cl-user)
(defpackage mcclim-panter 
  (:use :cl)
  (:import-from :clim
		#:make-text-style		
		#:present
		#:define-application-frame
		#:labelling
		#:horizontally
		#:vertically
		#:make-application-frame
		#:find-pane-named
		#:run-frame-top-level
		#:frame-exit
		#:*application-frame*
		#:with-drawing-options
		#:with-text-style
		#:+red+
		#:+black+
		#:SURROUNDING-OUTPUT-WITH-BORDER
		#:STREAM-CURSOR-POSITION
		#:STREAM-INCREMENT-CURSOR-POSITION
		#:GADGET-VALUE
		#:REDISPLAY-FRAME-PANE
		#:GADGET-LABEL
		#:FIND-FRAME-MANAGER
		#:WITH-RADIO-BOX
		#:RADIO-BOX-CURRENT-SELECTION
		)
  (:import-from :cl-ppcre
		#:create-scanner
		#:regex-apropos-list
		#:scan
		)
  (:export
   :run-panter
   ))

(in-package :mcclim-panter)
