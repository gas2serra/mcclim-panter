;;; Navigator
;;; ============================================================================
;;; Entry point is to use CLIMACS-GUI::COM-NAVIGATOR via M-C-s. Click on symbol
;;; values and press the return button to return them.

(in-package :mcclim-panter-apropos)

(defparameter *apropos-navigator-heading-text-style* (clim:make-text-style
						     nil
						     :bold 12))

(defvar *return-values* nil)

(clim:define-application-frame apropos-navigator ()
  ((return-values :initform nil)
   (syntax-error :initform nil)
   (selected-result-options :initform '(:fully-qualified))
   (selected-output-option :initform ':selection)
   (iapropos :initform (make-instance 'iapropos)))
  (:menu-bar nil)
  (:panes
   (symbol-regex-text-field :text-field
			    :value-changed-callback '%update-matching-symbols)
   (package-regex-text-field :text-field
			     :value-changed-callback '%update-matching-packages)
   (symbol-result-display :application
			  :incremental-redisplay t
			  :display-function '%render-symbol-result
			  :scroll-bars :vertical
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :min-width 300)
   (package-result-display :application
			  :incremental-redisplay t
			  :display-function '%render-package-result
			  :scroll-bars :vertical
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :min-width 300)
   (output-display :application
		   :incremental-redisplay t
		   :display-function '%render-output
		   :scroll-bars :vertical
		   :end-of-page-action :allow
		   :min-width 600)
   (result-options
    (clim:with-radio-box (:type :some-of
			  :orientation :horizontal
			  :value-changed-callback '%update-result-options)
      (clim:radio-box-current-selection "fully-qualified")))
   (output-option
    (clim:with-radio-box (;;:type :some-of
			  :orientation :horizontal
			  :value-changed-callback '%update-output-option)
      (clim:radio-box-current-selection "selection")
      "documentation" "location" "description" "object"))
   (external-option
    (clim:with-radio-box (:orientation :horizontal :value-changed-callback '%update-external-option)
      "yes"
      "no"
      (clim:radio-box-current-selection "nil")))
   (documentation-option
    (clim:with-radio-box (:orientation :horizontal :value-changed-callback '%update-documentation-option)
      "yes"
      "no"
      (clim:radio-box-current-selection "nil")))
   (bound-to-option
    (clim:with-radio-box (:orientation :vertical
			  :value-changed-callback '%update-bound-to-option)
      (clim:radio-box-current-selection "nil") "variable"
      "function" "class" "generic-function" "macro"
      "setf" "type"))
   (subclass-option :option-pane
		    :value nil
		    :items (list nil 'clim:pane 'clim:application-frame)
		    :value-changed-callback #'%update-subclass-option)
   (metaclass-option :option-pane
		     :value nil
		     :items (list nil 'climi::presentation-type-class)
		     :value-changed-callback #'%update-metaclass-option)
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
			   :label "packages"))
  (:layouts
   (:default
       (clim:horizontally nil
	 (clim:vertically nil
	   (clim:labelling (:label "Symbol")
	     (clim:vertically nil
	       (clim:labelling (:label "bound to")
		 bound-to-option)
	       (clim:labelling (:label "external")
		 external-option)
	       (clim:labelling (:label "documentation")
		 documentation-option)))
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
		    (clim:horizontally nil
		      package-result-display
		      symbol-result-display))))
	   (1/3 (clim:labelling (:label "Output")
		  (clim:vertically nil
		    output-option
		    output-display)))
	   (clim:vertically nil
	     (clim:labelling (:label "symbol apropos" :align-x :center)
	       symbol-regex-text-field)
	     (clim:labelling (:label "package apropos" :align-x :center)
	       package-regex-text-field)))))))

;;;
;;; callbacks
;;;

(defun %update-matching-packages (this-gadget value)
  (declare (ignore this-gadget))
  (with-slots (iapropos syntax-error) clim:*application-frame*
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf  syntax-error condition)
			(%maybe-update-result-display)
			(return-from %update-matching-packages))))
      (setf syntax-error nil)
      (setf (iapropos-package-text iapropos) value)
      (%maybe-update-result-display))))

(defun %update-matching-symbols (this-gadget value)
  (declare (ignore this-gadget))
  (with-slots (iapropos syntax-error) clim:*application-frame*
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf  syntax-error condition)
			(%maybe-update-result-display)
			(return-from %update-matching-symbols))))
      (setf syntax-error nil)
      (setf (iapropos-text iapropos) value))
    (%maybe-update-result-display)))

(defun %update-bound-to-option (this-gadget selected-gadget)
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
      (clim:activate-gadget (clim:find-pane-named clim:*application-frame* 'documentation-option))
      (clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'documentation-option)))
  (%maybe-update-result-display))

(defun %update-external-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (if (string= (clim:gadget-label selected-gadget) "nil")
	(setf (iapropos-external-yes/no iapropos) nil)
	(setf (iapropos-external-yes/no iapropos)
	      (intern (string-upcase (clim:gadget-label selected-gadget)) :keyword))))
  (%maybe-update-result-display))

(defun %update-documentation-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (if (string= (clim:gadget-label selected-gadget) "nil")
	(setf (iapropos-documentation-yes/no iapropos) nil)
	(setf (iapropos-documentation-yes/no iapropos)
	      (intern (string-upcase (clim:gadget-label selected-gadget)) :keyword))))
  (%maybe-update-result-display))

(defun %update-subclass-option (this-gadget selected-value)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (setf (iapropos-subclass-of iapropos) 
	  selected-value))
  (%maybe-update-result-display))

(defun %update-metaclass-option (this-gadget selected-value)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (setf (iapropos-metaclass-of iapropos) 
	  selected-value))
  (%maybe-update-result-display))

(defun %update-output-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (selected-output-option) clim:*application-frame*
    (setf selected-output-option 
	  (intern (string-upcase (clim:gadget-label selected-gadget)) :keyword)))
  (%maybe-update-output-display))

(defun %update-result-options (this-gadget selected-gadgets)
  (declare (ignore this-gadget))
  (with-slots (selected-result-options) clim:*application-frame*
    (setf selected-result-options nil)
    (dolist (sg selected-gadgets)
      (push 
       (intern (string-upcase (clim:gadget-label sg)) :keyword)
       selected-result-options)))
  (%maybe-update-result-display))

(defun %maybe-update-result-display (&rest _)
  (declare (ignore _))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'package-result-display)
    (clim:redisplay-frame-pane clim:*application-frame* anaphora:it :force-p t))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'symbol-result-display)
    (clim:redisplay-frame-pane clim:*application-frame* anaphora:it :force-p t)))


(defun %maybe-update-output-display (&rest _)
  (declare (ignore _))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'output-display)
		  (clim:redisplay-frame-pane clim:*application-frame* anaphora:it :force-p t)))


;;;
;;; render functions
;;;


(defun %print-heading-text (pane text &optional (x-offset 0))
  (fresh-line pane)
  (clim:stream-increment-cursor-position pane (+ x-offset 10) 0)
  (clim:surrounding-output-with-border
      (pane :shape :underline :ink clim:+black+)
    (clim:with-text-style (pane *apropos-navigator-heading-text-style*)
      (princ text pane))))

(defun %print-text (pane text &optional (x-offset 0))
  (fresh-line pane)
  (clim:stream-increment-cursor-position pane (+ x-offset 10) 0)
  (princ text pane))

(defun take (n l)
  (subseq l 0 (if (< (length l) n) (if (> n (1- (length l))) (length l) (1- (length l))) n)))

(defun %render-output (frame pane)
  (declare (ignore frame))
  (with-slots (return-values selected-output-option iapropos) clim:*application-frame*
    (flet ((print-symbol (sym type opt)
	     
	     (ccase opt
	       (:selection
		(%print-heading-text pane (format nil "Selected symbols"))
		(let ((*print-escape* t))
		  (dolist (v return-values)
		    (%print-text pane (format nil "~S~%" v)))))
	       (:object
		(%print-heading-text pane (format nil "Object (~A)" type))
		(%print-text pane (symbol-object sym type)))
	       (:location
		(%print-heading-text pane (format nil "Location (~A)" type))
		(%print-text pane (symbol-location (car return-values) type)))
	       (:documentation
		(%print-heading-text pane (format nil "Documentation (~A)" type))
		(%print-text pane (symbol-documentation (car return-values) type)))
	       (:description
		(%print-heading-text pane (format nil "Description (~A)" type))
		(%print-text pane (symbol-description (car return-values) type))))
	     (princ #\Newline pane)
	     (fresh-line pane)
	     (clim:stream-increment-cursor-position pane 10 10)
	     ))
      (setf (clim:stream-cursor-position pane) (values 10 10))
     
      (when return-values
	(let ((*print-escape* t))
	  (%print-heading-text pane (format nil "~S" (car return-values))))
	(if (iapropos-bound-to iapropos) 
	    (print-symbol (car return-values) (iapropos-bound-to iapropos) selected-output-option)
	      (if (eq selected-output-option :selection)
		  (print-symbol (car return-values) nil selected-output-option)
		  (dolist (type *symbol-bounding-types*)
		    (when (symbol-bound-to (car return-values) type)
		      (print-symbol (car return-values) type selected-output-option)))))))))

(defun %render-symbol-result (frame pane)
  (declare (ignore frame))
  (with-slots (iapropos syntax-error)
      clim:*application-frame*
    (when syntax-error
      (clim:with-drawing-options (pane :ink clim:+red+ :text-size 16)
	(let* ((*package* (find-package 'keyword)))
	  (format pane "Error encountered while trying to update apropos display~2%~A~2%Inspect ~S for more info" 
		  syntax-error
		  'syntax-error)))
      (return-from %render-symbol-result))
    (let* ((matching-symbols (iapropos-matching-symbols iapropos)))
      (labels ((matching-symbols-column ()
		 (let ((symbols-to-print (take 400 matching-symbols)))
		   (setf (clim:stream-cursor-position pane) (values 10 5))
		   (%print-heading-text pane
					(format nil "Symbols (~A/~A~A)"
						(length symbols-to-print)
						(length matching-symbols)
						(if (iapropos-result-overflow-p iapropos) "*" "")))
		   (progn (clim:stream-increment-cursor-position pane 0 -8)
			  (dolist (sym symbols-to-print)
			    (fresh-line pane)
			    (clim:stream-increment-cursor-position pane 10 0)
			    (let ((*print-escape* t))
			      (clim:present sym 'symbol :stream pane)))))))
	(if (null matching-symbols)
	    (%print-text pane "; no results")
	    (matching-symbols-column))))))

(defun %render-package-result (frame pane)
  (declare (ignore frame))
  (with-slots (iapropos syntax-error)
      clim:*application-frame*
    (when syntax-error
      (clim:with-drawing-options (pane :ink clim:+red+ :text-size 16)
	(let* ((*package* (find-package 'keyword)))
	  (format pane "Error encountered while trying to update apropos display~2%~A~2%Inspect ~S for more info" 
		  syntax-error
		  'syntax-error)))
      (return-from %render-package-result))
    (let* ((matching-packages (iapropos-matching-packages iapropos)))
      (labels ((matching-packages-column ()
		 (setf (clim:stream-cursor-position pane) (values 10 5))
		 (%print-heading-text pane (format nil "Packages (~A)" (length matching-packages)))
		 (dolist (package matching-packages)
		   (fresh-line pane)
		   (clim:stream-increment-cursor-position pane 10 0)
		   (princ (package-name package) pane))))
	(if (null matching-packages)
	    (%print-text pane "; no results")
	    (matching-packages-column))))))
	


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

(define-apropos-navigator-command (com-quit-and-return-values :menu t
						      :name t
						      :keystroke ((:meta #\q)))
    ()
  (with-slots (return-values) clim:*application-frame*
    (setf *return-values* (remove-duplicates return-values))
    (clim:frame-exit clim:*application-frame*)))

(define-apropos-navigator-command (com-select-symbol-for-return :name t)
    ((sym 'symbol :gesture :select))
  (with-slots (return-values) clim:*application-frame*
    (push sym return-values))
  (%maybe-update-output-display))

;;;
;;; run
;;;

(defun run-apropos-navigator ()
  (let ((*return-values* nil))
    (let* ((frame (clim:make-application-frame 'apropos-navigator)))
      (setf (clim:frame-current-layout frame) :default) 
      (setf (getf (slot-value frame 'clim-internals::properties) 'clim-clx::focus)
	    (clim:find-pane-named frame 'package-regex-text-field))
      ;;where?
      ;;(clim:deactivate-gadget (clim:find-pane-named frame 'subclass-option))
      ;;(clim:deactivate-gadget (clim:find-pane-named frame 'metaclass-option))
      ;;(clim:deactivate-gadget (clim:find-pane-named frame 'documentation-option))
      (clim:run-frame-top-level frame :name "apropos-navigator"))
    *return-values*))

;;;
;;; climacs
;;;
#|
(in-package climacs-gui)

(defvar return-point nil)

(define-command (com-apropos-navigator
                 :name t 
                 :command-table drei-lisp-syntax::lisp-table) ()
  (setf return-point (climacs-gui::point)
        climi::return-values nil)
  (flexichain:insert-sequence climacs-gui::return-point (write-to-string
							 (climi::run-apropos-navigator))))

(esa-io::set-key 'com-apropos-navigator 'drei-lisp-syntax::lisp-table '((#\s :meta :control)))
|#

;; listener
#|
(CLIM-LISTENER::DEFINE-LISTENER-COMMAND (COM-RUN-APROPOS-NAVIGATOR :NAME T)
    NIL
  (RUN-OR-FOCUS-APROPOS-NAVIGATOR))
|#

;;;
;;; stumpwm
;;;

#|
(DEFUN RUN-OR-FOCUS-APROPOS-NAVIGATOR ()
  ;;(ANAPHORA:AIF (STUMPWM::WINDOW-BY-NAME "APROPOS-NAVIGATOR") (STUMPWM:SELECT-WINDOW (STUMPWM::WINDOW-NAME ANAPHORA:IT))
  ;;              (BORDEAUX-THREADS:MAKE-THREAD 'RUN-APROPOS-NAVIGATOR :NAME "APROPOS-NAVIGATOR")))
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
