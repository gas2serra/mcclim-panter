(in-package :mcclim-panter)

(clim-listener::define-listener-command (com-run-apropos-navigator :name t)
    nil
  (run-apropos-navigator))

(clim-listener::define-listener-command (com-run-task-manager :name t)
    nil
  (run-task-manager))

