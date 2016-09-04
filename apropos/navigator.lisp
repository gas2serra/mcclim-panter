;;; Navigator
;;; ============================================================================
;;; Entry point is to use CLIMACS-GUI::COM-NAVIGATOR via M-C-s. Click on symbol
;;; values and press the return button to return them.

(in-package :mcclim-panter-apropos)

(defparameter *navigator-column-heading-text-style* (clim:make-text-style
						     nil
						     :bold 12))

(defvar *return-values* nil)

(clim:define-application-frame navigator ()
  ((return-values :initform nil)
   (syntax-error :initform nil)
   (selected-result-options :initform '(:fully-qualified))
   (selected-output-options :initform '(:selection))
   (iapropos :initform (make-instance 'iapropos)))
  (:menu-bar nil)
  (:panes (package-apropos-text-field :text-field
				      :value-changed-callback '%update-matching-packages)
	  (apropos-text-field :text-field
			      :value-changed-callback '%update-matching-symbols)
	  (apropos-display :application
			   :incremental-redisplay t
			   :display-function 'render-apropos
			   :scroll-bars :vertical
			   :end-of-page-action :allow
			   :min-width 600)
	  (output-display :application
				 :incremental-redisplay t
				 :display-function 'render-output
				 :scroll-bars :vertical
				 :end-of-page-action :allow
				 :min-width 600)
	  (result-options
	   (clim:with-radio-box (:type :some-of
				       :orientation :horizontal
				       :value-changed-callback '%update-result-options)
	     (clim:radio-box-current-selection "fully-qualified")))
	  (external-radio
	   (clim:with-radio-box (:orientation :horizontal :value-changed-callback '%update-external)
	     "yes"
	     "no"
	     (clim:radio-box-current-selection "nil")))
	  (documentation-radio
	   (clim:with-radio-box (:orientation :horizontal :value-changed-callback '%update-documentation)
	     "yes"
	     "no"
	     (clim:radio-box-current-selection "nil")))
	  (include-command-internals-radio
	   (clim:with-radio-box (:orientation :horizontal :value-changed-callback '%maybe-update-apropos-display)
	     "t"
	     (clim:radio-box-current-selection "nil")))
	  (apropos-bound-filter-radio
	   (clim:with-radio-box (:orientation :vertical
				 :value-changed-callback '%update-navigator-bound-filter)
	     (clim:radio-box-current-selection "nil") "variable"
	     "function" "class" "generic-function" "macro"
	     "setf" "type"))
	  (return-selected-symbols-button :push-button
					  :id :selected-symbols
					  :activate-callback #'quit-and-return-values
					  :label "selected symbols")
	  (return-symbols-button :push-button
				 :id :symbols
				 :activate-callback #'quit-and-return-values
				 :label "symbols")
	  (return-packages-button :push-button
				  :id :packages
				  :activate-callback #'quit-and-return-values
				  :label "packages")
	  (subclass-option :option-pane
			   :value nil
			   :items (list nil 'clim:pane 'clim:application-frame)
			   :value-changed-callback #'%update-subclass-option)
	  (metaclass-option :option-pane
			    :value nil
			    ;;:items (list nil 'climi::presentation-type-class)
			    :items (list nil)
			    :value-changed-callback #'%update-metaclass-option)
	  (output-options
	   (clim:with-radio-box (:type :some-of
				 :orientation :horizontal
				 :value-changed-callback '%update-output-options)
	     (clim:radio-box-current-selection "selection")
	     "documentation" "location" "description" "object")))
  (:layouts
   (:default
       (clim:vertically nil
	 (clim:horizontally nil
	   (clim:vertically nil
	     (clim:labelling (:label "Symbol")
	       (clim:vertically nil
		 (clim:labelling (:label "bound to")
		   apropos-bound-filter-radio)
		 (clim:labelling (:label "external")
		   external-radio)
	       	 (clim:labelling (:label "documentation")
		   documentation-radio)))
	     (clim:labelling (:label "Class")
		  (clim:vertically nil
		    (clim:labelling (:label "subclass of")
		      subclass-option)
		    (clim:labelling (:label "metaclass of")
		      metaclass-option)))
	     (clim:labelling (:label "Quit and return")
	       (clim:vertically nil
		 return-selected-symbols-button
		 return-symbols-button
		 return-packages-button)))
	   (clim:vertically nil
	     (2/3 (clim:labelling (:label "Results")
		    (clim:vertically nil
		      result-options
		      apropos-display)))
	     (1/3 (clim:labelling (:label "Output")
		    (clim:vertically nil
		      output-options
		      output-display)))
	     (clim:vertically nil
	       (clim:labelling (:label "symbol apropos" :align-x :center)
		 apropos-text-field)
	       (clim:labelling (:label "package apropos" :align-x :center)
		package-apropos-text-field))))))))




;;;
;;; callbacks
;;;

(defun %update-matching-packages (this-gadget value)
  (declare (ignore this-gadget))
  (with-slots (iapropos syntax-error) clim:*application-frame*
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf  syntax-error condition)
			(%maybe-update-apropos-display)
			(return-from %update-matching-packages))))
      (setf syntax-error nil)
      (setf (iapropos-package-text iapropos) value)
      (%maybe-update-apropos-display))))

(defun %update-matching-symbols (this-gadget value)
  (declare (ignore this-gadget))
  (with-slots (iapropos syntax-error) clim:*application-frame*
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf  syntax-error condition)
			(%maybe-update-apropos-display)
			(return-from %update-matching-symbols))))
      (setf syntax-error nil)
      (setf (iapropos-text iapropos) value))
    (%maybe-update-apropos-display)))

(defun %update-navigator-bound-filter (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (if (string= (clim:gadget-label selected-gadget) "nil")
	(setf (iapropos-bound-to iapropos) nil)
	(setf (iapropos-bound-to iapropos) (intern (string-upcase (clim:gadget-label selected-gadget)) :keyword))))
  (if (string= (clim:gadget-label selected-gadget) "class")
      (progn
	(clim:activate-gadget (clim:find-pane-named clim:*application-frame* 'subclass-option))
	(clim:activate-gadget (clim:find-pane-named clim:*application-frame* 'metaclass-option)))
      (progn
	(clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'subclass-option))
	(clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'metaclass-option))))
  (if (string/= (clim:gadget-label selected-gadget) "nil")
      (clim:activate-gadget (clim:find-pane-named clim:*application-frame* 'documentation-radio))
      (clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'documentation-radio)))
  (%maybe-update-apropos-display))

(defun %update-external (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (if (string= (clim:gadget-label selected-gadget) "nil")
	(setf (iapropos-external-yes/no iapropos) nil)
	(setf (iapropos-external-yes/no iapropos)
	      (intern (string-upcase (clim:gadget-label selected-gadget)) :keyword))))
  (%maybe-update-apropos-display))

(defun %update-documentation (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (if (string= (clim:gadget-label selected-gadget) "nil")
	(setf (iapropos-documentation-yes/no iapropos) nil)
	(setf (iapropos-documentation-yes/no iapropos)
	      (intern (string-upcase (clim:gadget-label selected-gadget)) :keyword))))
  (%maybe-update-apropos-display))

(defun %update-subclass-option (this-gadget selected-value)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (setf (iapropos-subclass-of iapropos) 
	  selected-value))
  (%maybe-update-apropos-display))

(defun %update-metaclass-option (this-gadget selected-value)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (setf (iapropos-metaclass-of iapropos) 
	  selected-value))
  (%maybe-update-apropos-display))

(defun %update-output-options (this-gadget selected-gadgets)
  (declare (ignore this-gadget))
  (with-slots (selected-output-options) clim:*application-frame*
    (setf selected-output-options nil)
    (dolist (sg  selected-gadgets)
      (push 
       (intern (string-upcase (clim:gadget-label sg)) :keyword)
       selected-output-options)))
  (%maybe-update-output-display))

(defun %update-result-options (this-gadget selected-gadgets)
  (declare (ignore this-gadget))
  (with-slots (selected-result-options) clim:*application-frame*
    (setf selected-result-options nil)
    (dolist (sg selected-gadgets)
      (push 
       (intern (string-upcase (clim:gadget-label sg)) :keyword)
       selected-result-options)))
  (%maybe-update-apropos-display))

(defun %maybe-update-apropos-display (&rest _)
  (declare (ignore _))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'apropos-display)
		  (clim:redisplay-frame-pane clim:*application-frame* anaphora:it :force-p t)))

(defun %maybe-update-output-display (&rest _)
  (declare (ignore _))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'output-display)
		  (clim:redisplay-frame-pane clim:*application-frame* anaphora:it :force-p t)))


;;;
;;; render functions
;;;

(defun take (n l)
  (subseq l 0 (if (< (length l) n) (if (> n (1- (length l))) (length l) (1- (length l))) n)))

(defun render-output (frame pane)
  (with-slots (return-values selected-output-options iapropos) clim:*application-frame*
    (setf (clim:stream-cursor-position pane) (values 10 10))
    (when return-values
      (dolist (s selected-output-options)
	(ccase s
	  (:selection
	   (format pane "Selected symbols~%-----~%~A~%~%" return-values))
	  (:object
	   (when (iapropos-bound-to iapropos)
	     (fresh-line pane)
	     (clim:stream-increment-cursor-position pane 10 0)
	     (format pane "Object~%-----~%~A~%~%"
		     (symbol-object (car return-values) (iapropos-bound-to iapropos)))))
	  (:location
	   (when (iapropos-bound-to iapropos)
	     (fresh-line pane)
	     (clim:stream-increment-cursor-position pane 10 0)
	     (format pane "Location~%-----~%~A~%~%"
		     (symbol-location (car return-values) (iapropos-bound-to iapropos)))))
	  (:documentation
	   (fresh-line pane)
	   (clim:stream-increment-cursor-position pane 10 0)
	   (format pane "Documentation~%-----~%~A~%~%"
		   (symbol-documentation (car return-values) (iapropos-bound-to iapropos))))
	  (:description
	   (fresh-line pane)
	   (format pane "Description~%-----~%~A~%~%"
		   (symbol-description (car return-values) (iapropos-bound-to iapropos)))))))))

(defun render-apropos (frame pane)
  ;; TODO
  ;; - display multiple columns of matching symbols according, CLASSES, SLOTS etc.
  ;; - when the change is a single character, filter existing search, etc.
  ;; - index SBCL symbol table each intern..
  ;; - clim-listener::apropos-present-symbol
  ;; - (cl-ppcre::regex-apropos-aux ("regex" packages t))
  (with-slots (iapropos syntax-error)
      clim:*application-frame*
    
    (when syntax-error
      (clim:with-drawing-options (pane :ink clim:+red+ :text-size 16)
	(let* ((*package* (find-package 'keyword)))
	  (format pane "Error encountered while trying to update apropos display~2%~A~2%Inspect ~S for more info" 
		  syntax-error
		  'syntax-error)))
      (return-from render-apropos))
    
    (let* ((pane (clim:find-pane-named frame 'apropos-display))
	   (matching-symbols (iapropos-matching-symbols iapropos))
	   (matching-packages (iapropos-matching-packages iapropos))
	   (symbols-x-offset 300))
      (labels ((set-offset-start-position (&key (y 5)) 
		 (setf (clim:stream-cursor-position pane) (values 10 y)))
	       
	       (matching-packages-column ()
		 ;; (length (list-all-packages)) => 397 so we'll just display them all..
		 (set-offset-start-position)
		 (clim:surrounding-output-with-border
		     (pane :shape :underline :ink clim:+black+)
		   (clim:with-text-style (pane *navigator-column-heading-text-style*)
		     (princ "Packages" pane)))
		 (clim:stream-increment-cursor-position pane 0 -8)                    
		 (dolist (package matching-packages)
		   (fresh-line pane)
		   (clim:stream-increment-cursor-position pane 10 0)
		   (princ (package-name package) pane)))
	       
	       (matching-symbols-column ()
		 (let ((symbols-to-print (take 400 matching-symbols)))
		   (setf (clim:stream-cursor-position pane) (values symbols-x-offset 5))
		   (clim:surrounding-output-with-border
		       (pane :shape :underline :ink clim:+black+)
		     (clim:with-text-style (pane *navigator-column-heading-text-style*)
		       (princ (format nil "Symbols (~A/~A~A)"
				      (length symbols-to-print)
				      (length matching-symbols)
				      (if (iapropos-result-overflow-p iapropos) "*" ""))
			      pane)))
		   (progn (clim:stream-increment-cursor-position pane 0 -8)
			  (dolist (sym symbols-to-print)
			    (fresh-line pane)
			    (clim:stream-increment-cursor-position pane symbols-x-offset 0)
			    (clim:present sym 'symbol :stream pane))))))
	(if (and (null matching-packages) (null matching-symbols))
	    (progn (set-offset-start-position)
		   (princ "; no results" pane))
	    (progn (matching-packages-column)
		   (matching-symbols-column)))))))

;;;
;;; exit
;;;

(defun quit-and-return-values (this-gadget)
  (with-slots (return-values iapropos) clim:*application-frame*
    (setf *return-values* 
	  (ccase (clim:gadget-id this-gadget)
	    (:selected-symbols
	     (remove-duplicates return-values))
	    (:symbols
	     (iapropos-matching-symbols iapropos))
	    (:packages
	     (iapropos-matching-packages iapropos))))
    (clim:frame-exit clim:*application-frame*)))

;;;
;;; commands
;;;

(define-navigator-command (com-quit-and-return-values :menu t
						      :name t
						      :keystroke ((:meta #\q)))
    ()
  (with-slots (return-values) clim:*application-frame*
    (setf *return-values* (remove-duplicates return-values))
    (clim:frame-exit clim:*application-frame*)))

(define-navigator-command (com-select-symbol-for-return :name t)
    ((sym 'symbol :gesture :select))
  (with-slots (return-values) clim:*application-frame*
    (push sym return-values))
  (%maybe-update-output-display))

;;;
;;; run
;;;

(defun run-navigator ()
  (let ((*return-values* nil))
    (let* ((frame (clim:make-application-frame 'navigator)))
      (setf (clim:frame-current-layout frame) :default) 
      (setf (getf (slot-value frame 'clim-internals::properties) 'clim-clx::focus)
	    (clim:find-pane-named frame 'package-apropos-text-field))
      ;;where?
      ;;(clim:deactivate-gadget (clim:find-pane-named frame 'subclass-option))
      ;;(clim:deactivate-gadget (clim:find-pane-named frame 'metaclass-option))
      ;;(clim:deactivate-gadget (clim:find-pane-named frame 'documentation-radio))
      (clim:run-frame-top-level frame :name "navigator"))
    *return-values*))

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


 (swank:find-source-location-for-emacs '(:inspector "cons"))
(swank:find-definitions-for-emacs "cl:cons")
(("(DEFTYPE CONS)"
  (:LOCATION
   (:FILE
    "/home/csr21/src/lisp/sbcl-release-dir-20160830-dY7abU0Va/sbcl-1.3.9/src/compiler/generic/objdef.lisp")
   (:POSITION 1) NIL))
 ("(DEFUN CONS)"
  (:LOCATION
   (:FILE
    "/home/csr21/src/lisp/sbcl-release-dir-20160830-dY7abU0Va/sbcl-1.3.9/src/code/list.lisp")
   (:POSITION 1) (:SNIPPET "(defun CONS ")))
 ("(DEFCLASS CONS)"
  (:ERROR
   "Error: DEFINITION-SOURCE of class CONS did not contain meaningful information."))
 ("(:DEFOPTIMIZER CONS SB-C:IR2-CONVERT)"
  (:LOCATION
   (:FILE
    "/home/csr21/src/lisp/sbcl-release-dir-20160830-dY7abU0Va/sbcl-1.3.9/src/compiler/fun-info-funs.lisp")
   (:POSITION 1) NIL))
 ("(:DEFOPTIMIZER CONS SB-C::STACK-ALLOCATE-RESULT)"
  (:LOCATION
   (:FILE
    "/home/csr21/src/lisp/sbcl-release-dir-20160830-dY7abU0Va/sbcl-1.3.9/src/compiler/generic/vm-ir2tran.lisp")
   (:POSITION 1) NIL))
 ("(DECLAIM CONS
         SB-C:DEFKNOWN)"
  (:LOCATION
   (:FILE
    "/home/csr21/src/lisp/sbcl-release-dir-20160830-dY7abU0Va/sbcl-1.3.9/src/compiler/fndb.lisp")
   (:POSITION 1) NIL)))
CL-USER> 
|#
