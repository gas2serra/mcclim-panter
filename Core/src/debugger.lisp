(in-package :panter-internals)

(defvar *debugger* nil)

(defvar *panter-debugger-hook*
  #'(lambda (condition me-or-my-encapsulation)
      (funcall *debugger*
	       condition
	       me-or-my-encapsulation)))
