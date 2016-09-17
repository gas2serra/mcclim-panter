(in-package :common-lisp-user)

(defpackage :panter
  (:use :common-lisp)
  (:nicknames :pant)
  (:export
   #:*debugger*
   ))

(defpackage :panter-extensions
  (:use :panter :common-lisp)
  (:export
   ))

(defpackage :panter-sys
  (:use :common-lisp)
  (:export
   #:copy-to-x11-clipboard
   #:paste-from-x11-clipboard
   #:copy-to-kill-ring
   ))

(defpackage :panter-apps
  (:use :panter :panter-extensions :panter-sys :common-lisp)
  (:export
   #:run-apropos-navigator
   #:debugger
   #:run-task-manager))
   
(defpackage :panter-internals
  (:use :panter :panter-extensions :panter-sys :common-lisp)
  (:nicknames :panti))

(defpackage :panter-user
  (:use :panter :panter-apps :common-lisp))
