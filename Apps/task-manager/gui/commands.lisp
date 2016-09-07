(in-package :mcclim-panter-task-manager)

;;;
;;; commands
;;;

(clim:define-command (com-quit :name "Quit"
			       :command-table task-manager
			       :menu t
			       :provide-output-destination-keyword nil)
    ()
  (clim:frame-exit clim:*application-frame*))

;;;
;;; command tables
;;;


(clim:define-command-table task-manager-commands)

;;;
;;; commands
;;;

(clim:define-command (com-repl :name "repl"
			       :command-table task-manager-commands
			       :menu t
			       :provide-output-destination-keyword nil)
    ()
  (clim-listener:run-listener :new-process t))


(clim:define-command (com-list-frames :name "ls frames"
			       :command-table task-manager-commands
			       :menu t)
    ()
  (clim:map-over-frames #'(lambda (x) (format t "~A~%" x))))

(clim:define-command (com-list-processes :name "ls processes"
			       :command-table task-manager-commands
			       :menu t)
    ()
  (dolist (p (clim-sys:all-processes))
    (fresh-line)
    (clim:present p 'thread :view clim:+textual-view+)))

;;; gesture :select

(define-task-manager-command (com-thread-break :name "Break")
    ((thread 'thread :gesture :select))
  (bt:interrupt-thread thread #'(lambda () (break))))

(define-task-manager-command (com-thread-inspect :name "Inspect application frame")
    ((thread 'thread :gesture :select))
  (bt:interrupt-thread thread
		       #'(lambda ()
			   (clouseau:inspector clim:*application-frame*))))

  
