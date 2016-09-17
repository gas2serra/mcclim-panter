(in-package :panter-internals)

;;;;
;;;; Debugging
;;;;

(defvar *debugger* nil
  "The current debugger")

;;; functions

(defun use-debugger (debugger)
  (setq *debugger* debugger))

;;; macros

(defmacro with-debugger ((debugger) &body body)
  `(let ((*debugger* ,debugger))
     ,@body))

;;;
;;; debugger hook
;;;

(defvar *panter-debugger-hook*
  #'(lambda (condition me-or-my-encapsulation)
      (funcall *debugger*
	       condition
	       me-or-my-encapsulation)))
