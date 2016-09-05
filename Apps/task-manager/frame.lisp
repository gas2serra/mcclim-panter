(in-package :mcclim-panter-task-manager)

(clim:define-application-frame task-manager (clim:standard-application-frame)
  ()
  (:panes
   (app :application :display-time nil)
   (doc :pointer-documentation) 
   (interact :interactor))
  (:command-table
   (task-manager
    :inherit-from (task-manager-commands)
    :menu (("Tasks" :menu task-manager-commands))))
  (:menu-bar t)
  (:layouts (default
		(clim:vertically ()
		  app
		  doc
		  interact))))


(clim:define-command (com-quit :name "Quit"
			       :command-table task-manager
			       :menu t
			       :provide-output-destination-keyword nil)
    ()
  (clim:frame-exit clim:*application-frame*))
