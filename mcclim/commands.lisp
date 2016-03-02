(in-package :mcclim-panter)

(clim:define-command-table panter-commands)

(clim:define-command (com-listener :name "debug-listener" :command-table panter-commands :menu t)
    ()
  (clim-listener:run-listener :new-process t))

(clim:define-command (com-repl :name "debug-repl" :command-table panter-commands :menu t)
    ()
  (clim-listener:run-listener :new-process t))
