(in-package :panter-apps)

(defun initialize ()
  (setf panter:*debugger* #'debugger))

(initialize)
