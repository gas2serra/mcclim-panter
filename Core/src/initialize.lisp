(in-package :panter-internals)

(defun initialize ()
  (setf *debugger* *debugger-hook*)
  (setf *debugger-hook* *panter-debugger-hook*))
