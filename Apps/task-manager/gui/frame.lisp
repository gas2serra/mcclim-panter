(in-package :mcclim-panter-task-manager)

(clim:define-application-frame task-manager (clim:standard-application-frame)
  ()
  (:panes
   (frame-display :application
		  :display-function #'%render-frame-display
		  :display-time nil)
   (thread-display :application
		   :display-function #'%render-thread-display
		   :display-time nil)
   (doc :pointer-documentation) 
   (interact :interactor))
  (:command-table
   (task-manager))
  (:menu-bar t)
  (:layouts (default
		(clim:vertically ()
		  (2/3
		   (clim:horizontally nil
		     (clim:labelling (:label "Frames")
		       frame-display)
		     (clim:labelling (:label "Threads")
		       thread-display)))
		  (1/3
		   (clim:labelling (:label "Interactor")
		     interact))
		  doc))))

;;;
;;; render functions
;;;

(defun %render-frame-display (frame pane)
  (declare (ignore frame))
  (clim:map-over-frames #'(lambda (frame)
			    (fresh-line pane)
			    (clim:stream-increment-cursor-position pane 5 3)
			    (clim:present frame
					  'clim:application-frame
					  :view clim:+textual-view+
					  :stream pane))))

#|
(dolist (package matching-packages)
  (fresh-line pane)
  (clim:stream-increment-cursor-position pane 5 0)
  (clim:present package 'package :stream pane :view clim:+textual-view+))))))
|#

(defun %render-thread-display (frame pane)
  (declare (ignore frame))
  (dolist (thread (clim-sys:all-processes))
    (fresh-line pane)
    (clim:stream-increment-cursor-position pane 5 3)
    (clim:present thread 'thread
		  :view clim:+textual-view+
		  :stream pane)))

