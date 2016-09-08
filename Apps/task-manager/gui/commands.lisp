(in-package :mcclim-panter-task-manager)

;;;
;;; task manager commands
;;;

(define-task-manager-command (com-quit :name "Quit"
					    :menu t
					    :provide-output-destination-keyword nil)
    ()
  (clim:frame-exit clim:*application-frame*))

;;; gesture :select and :help

(define-task-manager-command (com-application-frame-inspect
			      :name "Inspect Application Frame")
    ((frame 'clim:application-frame :gesture :select))
  (clouseau:inspector frame))

(define-task-manager-command (com-application-frame-break
			      :name "Break Application Frame")
    ((frame 'clim:application-frame :gesture :help))
  (dolist (thread (clim-sys:all-processes))
    (bt:interrupt-thread thread
			 #'(lambda ()
			     (when (and (boundp 'clim:*application-frame*)
					(eq clim:*application-frame* frame))
			       (break))))))

;;;
;;; command tables
;;;

(clim:define-command-table task-manager-commands)

;;;
;;; commands
;;;

(clim:define-command (com-repl :name "Launch Listener"
			       :command-table task-manager-commands
			       :menu t
			       :provide-output-destination-keyword nil)
    ()
  (clim-listener:run-listener :new-process t))


(clim:define-command (com-list-application-frames :name "List Application Frames"
						  :command-table task-manager-commands
						  :menu t)
    ()
  (fresh-line)
  (princ "*list application frames*")
  (clim:map-over-frames
   #'(lambda (frame)
       (fresh-line)
       (clim:present frame 'clim:application-frame :view clim:+textual-view+))))

(clim:define-command (com-clear-output-history :name "Clear Output History"
					       :command-table task-manager-commands
					       :menu t)
    ()
  (clim:window-clear *standard-output*))

