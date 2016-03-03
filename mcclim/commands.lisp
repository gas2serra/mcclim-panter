(in-package :mcclim-panter)

(clim:define-command-table panter-commands)

(clim:define-command (com-repl :name "repl"
			       :command-table panter-commands
			       :menu t
			       :provide-output-destination-keyword nil)
    ()
  (clim-listener:run-listener :new-process t))


(clim:define-command (com-list-frames :name "ls frames"
			       :command-table panter-commands
			       :menu t)
    ()
  (clim:map-over-frames #'(lambda (x) (format t "~A~%" x))))

(clim:define-command (com-list-processes :name "ls processes"
			       :command-table panter-commands
			       :menu t)
    ()
  (dolist (p (clim-sys:all-processes))
    (format t "~A~%" p)))

