(in-package :cl-user)

(defpackage mcclim-panter-debugger 
  (:use :cl :panter-apps)
  (:import-from :clim
		)
  (:export
   :debugger
   ))

(in-package :mcclim-panter-debugger)
