(in-package :mcclim-panter)

(clim:define-application-frame panter (clim:standard-application-frame)
  ()
  (:panes
   (app :application :display-time nil)
   (doc :pointer-documentation) 
   (interact :interactor))
  (:command-table
   (panter
    :inherit-from (panter-commands)
    :menu (("Panter" :menu panter-commands))))
  (:menu-bar t)
  (:layouts (default
		(clim:vertically ()
		  app
		  doc
		  interact))))


(clim:define-command (com-quit :name "Quit"
			       :command-table panter
			       :menu t
			       :provide-output-destination-keyword nil)
    ()
  (clim:frame-exit clim:*application-frame*))
