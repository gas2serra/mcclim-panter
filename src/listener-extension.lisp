(in-package :mcclim-panter)

(clim-listener::define-listener-command (com-run-apropos-navigator :name t)
    nil
  (run-apropos-navigator))

(clim-listener::define-listener-command (com-run-task-manager :name t)
    nil
  (run-task-manager))

(clim-listener::define-listener-command (com-toggle-debugger :name t) ()
  (setf *debugger-hook*
        (if (or (eq #'swank:swank-debugger-hook *debugger-hook*)
                (eq 'swank:swank-debugger-hook *debugger-hook*))
            (progn (format t "Enabled panter debugger~%") #'debugger)
            (progn (format t "Enabled swank debugger~%") #'swank::swank-debugger-hook))))
