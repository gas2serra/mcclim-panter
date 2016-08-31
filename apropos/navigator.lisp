;;; Navigator
;;; ============================================================================
;;; Entry point is to use CLIMACS-GUI::COM-NAVIGATOR via M-C-s. Click on symbol
;;; values and press the return button to return them.

(in-package :mcclim-panter-apropos)

(defparameter navigator-column-heading-text-style
  (clim:make-text-style
   nil
   :bold 12))
  
(defvar *return-values* nil)

(clim:define-application-frame navigator ()
  ((navigator-bound-filter :initform nil)
   (return-values :initform nil))
  (:menu-bar nil)
  (:panes (package-apropos-text-field :text-field
				      :value-changed-callback 'maybe-update-apropos-display)
	  (apropos-text-field :text-field
			      :value-changed-callback 'maybe-update-apropos-display)
	  (apropos-display :application
			   :incremental-redisplay t
			   :display-function 'render-apropos
			   :scroll-bars :vertical
			   :end-of-page-action :allow)
	  (fully-qualified-symbols-radio
	   (clim:with-radio-box (:orientation :horizontal :value-changed-callback 'maybe-update-apropos-display)
	     (clim:radio-box-current-selection "t")
	     "nil"))
	  (include-external-symbols-radio
	   (clim:with-radio-box (:orientation :horizontal :value-changed-callback 'maybe-update-apropos-display)
	     "t"
	     (clim:radio-box-current-selection "nil")))
	  (include-command-internals-radio
	   (clim:with-radio-box (:orientation :horizontal :value-changed-callback 'maybe-update-apropos-display)
	     "t"
	     (clim:radio-box-current-selection "nil")))
	  (apropos-bound-filter-radio
	   (clim:with-radio-box (:orientation :horizontal :value-changed-callback 'update-navigator-bound-filter)
	     (clim:radio-box-current-selection "nil")
	     "boundp"
	     "fboundp"))
	  (return-button :push-button
			 :activate-callback
			 (lambda (&rest _)
			   (declare (ignore _))
			   (com-quit-and-return-values-to-point)) :label
			   "quit and return selected values"))
  (:layouts
   (:default
       (clim:vertically nil
	 apropos-display
	 (clim:vertically nil
	   (clim:horizontally nil
	     return-button
	     (clim:labelling (:label "print fully qualified symbols?")
	       fully-qualified-symbols-radio)
	     (clim:labelling (:label "include external symbols?")
	       include-external-symbols-radio)
	     (clim:labelling (:label "include command internals?")
	       include-command-internals-radio)
	     (clim:labelling (:label "bound filter")
	       apropos-bound-filter-radio))
	   (clim:horizontally nil
	     (1/2
	      (clim:labelling (:label "symbol apropos" :align-x :center)
		apropos-text-field))
	     (1/2
	      (clim:labelling (:label "package apropos" :align-x :center)
		package-apropos-text-field))))))))

(defun run-navigator ()
  (let ((*return-values* nil))
    (let* ((frame (clim:make-application-frame 'navigator)))
      (setf (getf (slot-value frame 'clim-internals::properties) 'clim-clx::focus)
	    (clim:find-pane-named frame 'package-apropos-text-field))
      (clim:run-frame-top-level frame :name "navigator"))
    *return-values*))




(defun update-navigator-bound-filter (this-gadget selected-gadget)
  (with-slots (navigator-bound-filter) clim:*application-frame*
    (let* ((sym (read-from-string (clim:gadget-label selected-gadget))))
      (setf navigator-bound-filter sym))
    (maybe-update-apropos-display)))



(defun maybe-update-apropos-display (&rest _)
  (declare (ignore _))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'apropos-display)
    (clim:redisplay-frame-pane clim:*application-frame* anaphora:it :force-p t)))

(defun emptyp (o)
  (eq o nil))

(defun take (n l)
  (subseq l 0 (if (< (length l) n) (if (> n (1- (length l))) (length l) (1- (length l))) n)))

(defun render-apropos (frame pane)
  ;; TODO
  ;; - display multiple columns of matching symbols according, CLASSES, SLOTS etc.
  ;; - when the change is a single character, filter existing search, etc.
  ;; - index SBCL symbol table each intern..
  ;; - clim-listener::apropos-present-symbol
  ;; - (cl-ppcre::regex-apropos-aux ("regex" packages t))  
  (handler-bind ((error (lambda (condition)
                          ;;(setf navigator-error-condition condition)
                          (clim:with-drawing-options (pane :ink clim:+red+ :text-size 16)
                            (let* ((*package* (find-package 'keyword)))
                              (format pane "Error encountered while trying to update apropos display~2%~S~2%Inspect ~S for more info" 
                                      condition
                                      'condition
                                      )))
                          (return-from render-apropos))))
    (with-slots (navigator-bound-filter) clim:*application-frame*
      (let* ((pane (clim:find-pane-named frame 'apropos-display))
             (package-apropos-regex-string 
              (clim:gadget-value (clim:find-pane-named clim:*application-frame* 'package-apropos-text-field)))
             (apropos-regex-string 
              (clim:gadget-value (clim:find-pane-named clim:*application-frame* 'apropos-text-field)))
             (ignorable-length-apropos (<= (length apropos-regex-string) 3))
             (symbols-x-offset 200))
	(let* ((matching-packages (unless (emptyp package-apropos-regex-string)
				    (package-apropos-list package-apropos-regex-string)))
	       (matching-symbols (unless (or (emptyp apropos-regex-string) ignorable-length-apropos)
				   (symbol-apropos-list apropos-regex-string matching-packages))))
	  ;; if we previously had an error condition, clear it. If it throws again,
	  ;; see the HANDLER-BIND
	  ;;(and navigator-error-condition (setf navigator-error-condition nil))
	  (when matching-symbols
	    (when navigator-bound-filter 
	      (setf matching-symbols (remove-if-not #'(lambda (s)
							(funcall navigator-bound-filter (car s)))
						    matching-symbols)))
	    ;;(when display-external-symbols-only?
	    ;;  (setf matching-symbols (remove-if-not #'external-symbol? matching-symbols)))
	    ;;(unless display-command-internals?
	    ;;  (setf matching-symbols (remove-if #'command-internals-symbol? matching-symbols))))
	    )
	  
	  (labels ((set-offset-start-position (&key (y 5)) 
		     (setf (clim:stream-cursor-position pane) (values 10 y)))
		   
		   (matching-packages-column ()
		     ;; (length (list-all-packages)) => 397 so we'll just display them all..
		     (set-offset-start-position)
		     (clim:surrounding-output-with-border
			 (pane :shape :underline :ink clim:+black+)
		       (clim:with-text-style (pane navigator-column-heading-text-style)
			 (princ "Packages" pane)))
		     (clim:stream-increment-cursor-position pane 0 -8)                    
		     (dolist (package matching-packages)
		       (fresh-line pane)
		       (clim:stream-increment-cursor-position pane 10 0)
		       (princ (package-name package) pane)))
		   
		   (matching-symbols-column ()
		     (setf (clim:stream-cursor-position pane) (values symbols-x-offset 5))
		     (clim:surrounding-output-with-border
			 (pane :shape :underline :ink clim:+black+)
		       (clim:with-text-style (pane navigator-column-heading-text-style)
			 (princ "Symbols" pane)))
		     (if ignorable-length-apropos
			 (progn (fresh-line pane)
				(clim:stream-increment-cursor-position pane symbols-x-offset 0)
				(princ "; (<= 3 search-string-length), not searching" pane))
			 (progn (clim:stream-increment-cursor-position pane 0 -8)
				(dolist (sym (take 800 matching-symbols))
				  (fresh-line pane)
				  (clim:stream-increment-cursor-position pane symbols-x-offset 0)
				  (clim:present (second (second sym)) 'symbol :stream pane)
		
				  )))))
	    
	    (if (and (null matching-packages) (null matching-symbols))
		(progn (set-offset-start-position)
		       (princ "; no results" pane))
		(progn (matching-packages-column)
		       (matching-symbols-column)))))))))

(define-navigator-command (com-quit-and-return-values-to-point :menu t
                                                               :name t
                                                               :keystroke ((:meta #\q)))
    ()
  (with-slots (return-values) clim:*application-frame*
    (setf *return-values* return-values)
    (clim:frame-exit clim:*application-frame*)))

(define-navigator-command (com-select-symbol-for-return :name t)
    ((sym 'symbol :gesture :select))
  (with-slots (return-values) clim:*application-frame*
    (push sym return-values)))


;;;
;;; climacs
;;;
#|
(in-package climacs-gui)

(defvar return-point nil)

(define-command (com-navigator
                 :name t 
                 :command-table drei-lisp-syntax::lisp-table) ()
  (setf return-point (climacs-gui::point)
        climi::return-values nil)
  (flexichain:insert-sequence climacs-gui::return-point (write-to-string
							 (climi::run-navigator))))

(esa-io::set-key 'com-navigator 'drei-lisp-syntax::lisp-table '((#\s :meta :control)))
|#

;; listener
#|
(CLIM-LISTENER::DEFINE-LISTENER-COMMAND (COM-RUN-NAVIGATOR :NAME T)
    NIL
  (RUN-OR-FOCUS-NAVIGATOR))
|#

;;;
;;; stumpwm
;;;

#|
(DEFUN RUN-OR-FOCUS-NAVIGATOR ()
  ;;(ANAPHORA:AIF (STUMPWM::WINDOW-BY-NAME "NAVIGATOR") (STUMPWM:SELECT-WINDOW (STUMPWM::WINDOW-NAME ANAPHORA:IT))
  ;;              (BORDEAUX-THREADS:MAKE-THREAD 'RUN-NAVIGATOR :NAME "NAVIGATOR")))
  )
|#
