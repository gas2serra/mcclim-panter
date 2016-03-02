(in-package :mcclim-panter)

(defun run-panter (&key (new-process nil)
		     (width 790)
		     (height 550)
		     port
		     frame-manager
		     (pretty-name "Panter")
		     (process-name "panter"))
  (let* ((fm (or frame-manager (clim:find-frame-manager :port (or port (clim:find-port)))))
         (frame (clim:make-application-frame 'panter
					     :pretty-name pretty-name
					     :frame-manager fm
					     :width width
					     :height height)))
    (flet ((run () 
	     (unwind-protect (clim:run-frame-top-level frame)
	       (clim:disown-frame fm frame))))
      (if new-process
          (values (clim-sys:make-process #'run :name process-name)
                  frame)
          (run)))))		 
