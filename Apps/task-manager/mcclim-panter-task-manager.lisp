(in-package :cl-user)

(defpackage mcclim-panter-task-manager
  (:use :cl :panter-apps)
  (:import-from :clim
		)
  (:export
   :run-task-manager
   ))

(in-package :mcclim-panter-task-manager)
