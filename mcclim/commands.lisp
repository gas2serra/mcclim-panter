(in-package :mcclim-panter)

(clim:define-command-table panter-commands)

(clim:define-command (com-repl :name "repl"
			       :command-table panter-commands
			       :menu t
			       :provide-output-destination-keyword nil)
    ()
  (clim-listener:run-listener :new-process t))
