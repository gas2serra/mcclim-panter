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


