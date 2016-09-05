(in-package :cl-user)

(defpackage mcclim-panter
  (:use :common-lisp)
  (:nicknames :panter)
  (:import-from :mcclim-panter-apropos
		#:run-apropos-navigator)
  (:import-from :mcclim-panter-task-manager
		#:run-task-manager)
  (:import-from :mcclim-panter-debugger
		#:debugger)
  (:export
   :run-apropos-navigator
   :run-task-manager
   :debugger
   ))

(in-package :mcclim-panter)
