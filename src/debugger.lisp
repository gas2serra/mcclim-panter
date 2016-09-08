(in-package :mcclim-panter)


(defvar *debugger* #'debugger)

(defvar *panter-debugger-hook* #'(lambda (condition me-or-my-encapsulation)
				   (funcall *debugger* condition me-or-my-encapsulation)))

(clim-listener::define-listener-command (com-run-apropos-navigator :name t)
    nil
  (run-apropos-navigator))

(clim-listener::define-listener-command (com-run-task-manager :name t)
    nil
  (run-task-manager))

(clim-listener::define-listener-command (com-toggle-debugger :name t) ()
  (unless (eq *debugger-hook* *panter-debugger-hook*)
    (setf *debugger-hook* *panter-debugger-hook*))
  (setf *debugger*
        (if (or (eq #'swank:swank-debugger-hook *debugger*)
                (eq 'swank:swank-debugger-hook *debugger*))
            (progn (format t "Enabled panter debugger~%") #'debugger)
            (progn (format t "Enabled swank debugger~%") #'swank::swank-debugger-hook))))
