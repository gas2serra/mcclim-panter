(defpackage :panter-apps
  (:use :common-lisp)
  (:import-from #:mcclim-panter-apropos
		#:run-apropos-navigator)
  (:import-from #:mcclim-panter-debugger
		#:debugger)
  (:import-from #:mcclim-panter-task-manager
		#:run-task-manager)
  (:export
   #:run-apropos-navigator
   #:debugger
   #:run-task-manager))

