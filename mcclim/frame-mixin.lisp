(in-package :mcclim-panter)

(defclass panter-frame-mixin ()
  ((ori-command-table)))

(defmethod initialize-instance :after ((frame panter-frame-mixin) &rest initargs)
  (with-slots (ori-command-table) frame
    (setf ori-command-table (clim:frame-command-table frame))
    (setf (clim:frame-command-table frame)
	  (clim:make-command-table nil
				   :inherit-from (list
						  ori-command-table
						  (clim:find-command-table  'panter-commands))
				   :inherit-menu t))
    (unless (clim:find-menu-item "Panter" ori-command-table :errorp nil)
      (clim:add-menu-item-to-command-table ori-command-table "Panter"
					   :menu (clim:find-command-table 'panter-commands)))))

(defmethod clim:frame-exit :before ((frame panter-frame-mixin))
  (with-slots (ori-command-table) frame
    (when (clim:find-menu-item "Panter" ori-command-table :errorp nil)
      (clim:remove-menu-item-from-command-table ori-command-table "Panter"))))
