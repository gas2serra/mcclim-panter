;;; Navigator
;;; ============================================================================
;;; Entry point is to use CLIMACS-GUI::COM-NAVIGATOR via M-C-s. Click on symbol
;;; values and press the return button to return them.

(in-package :mcclim-panter-apropos)

(defvar *return-values* nil)

(clim:define-application-frame apropos-navigator ()
  ((selected-values :initform nil)
   (selected-result-options :initform '(:fully-qualified))
   (selected-output-option :initform ':selection)
   (selected-action-option :initform ':single)
   (symbol-view :initform +fully-qualified-symbol-view+)
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
			  :display-time nil
			  :scroll-bars :vertical
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :min-width 300)
   (package-result-display :application
			  :incremental-redisplay t
			  :display-function '%render-package-result
			  :display-time nil
			  :scroll-bars :vertical
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :min-width 300
			  :max-width 500)
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
		    :active nil
		    :items *apropos-navigator-subclas-of-options*
		    :name-key #'car
		    :value-key #'cdr	    
		    :value-changed-callback #'%update-subclass-option)
   (metaclass-option :option-pane
		     :value nil
		     :active nil
		     :items *apropos-navigator-metaclas-of-options*
		     :name-key #'car
		     :value-key #'cdr	
		     :value-changed-callback #'%update-metaclass-option)
   (filter-option :option-pane
		  :value nil
		  :items *apropos-navigator-filter-options*
		  :name-key #'car
		  :value-key #'cdr
		  :value-changed-callback #'%update-filter-option)
   (action-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-action-option)
      (clim:radio-box-current-selection "single") "multiple"))
   (return-action :push-button
		  :activate-callback #'return-action
		  :label "return")
   (copy-action :push-button
		:activate-callback #'copy-action
		:label "copy")
   (kill-ring-action :push-button
		     :activate-callback #'kill-ring-action
		     :label "kill-ring")
   (clear-action :push-button
		 :active nil
		 :activate-callback #'clear-action
		 :label "clear selection")
   (select-all-action :push-button
		      :active nil
		      :activate-callback #'select-all-action
		      :label "select all"))
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
	   (clim:labelling (:label "Filter")
	     filter-option)
	   (clim:labelling (:label "Selection")
	     action-option)
	   (clim:+fill+ (clim:labelling (:label "Actions")
			  (clim:vertically nil
			    return-action
			    copy-action
			    kill-ring-action
			    select-all-action
			    clear-action))))
	 (clim:+fill+
	  (clim:vertically nil
	    (2/3 (clim:labelling (:label "Results")
		   (clim:vertically nil
		     result-options
		     (clim:horizontally nil
		       (1/2 package-result-display)
		       (1/2 symbol-result-display)))))
	    (1/3 (clim:labelling (:label "Output")
		   (clim:vertically nil
		     output-option
		     output-display)))
	    (clim:vertically nil
	      (clim:labelling (:label "symbol apropos" :align-x :center)
		symbol-regex-text-field)
	      (clim:labelling (:label "package apropos" :align-x :center)
		package-regex-text-field))))))))

;;;
;;; callbacks
;;;

(defun %update-matching-packages (this-gadget value)
  (declare (ignore this-gadget))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'output-display)
    (clim:window-clear anaphora:it))
  (with-slots (iapropos) clim:*application-frame*
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(%print-error
			 (clim:find-pane-named clim:*application-frame* 'output-display)
			 condition)
			(%maybe-update-result-display)
			(return-from %update-matching-packages))))
      (setf (iapropos-package-text iapropos) value)
      (%maybe-update-result-display))))

(defun %update-matching-symbols (this-gadget value)
  (declare (ignore this-gadget))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'output-display)
    (clim:window-clear anaphora:it))
  (with-slots (iapropos) clim:*application-frame*
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(%print-error
			 (clim:find-pane-named clim:*application-frame* 'output-display)
			 condition)
			(%maybe-update-result-display)
			(return-from %update-matching-symbols))))
      (setf (iapropos-text iapropos) value)
      (%maybe-update-result-display))))

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

(defun %update-filter-option (this-gadget selected-value)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (setf (iapropos-filter-fn iapropos) 
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
  (with-slots (selected-result-options symbol-view) clim:*application-frame*
    (setf selected-result-options nil)
    (dolist (sg selected-gadgets)
      (push 
       (intern (string-upcase (clim:gadget-label sg)) :keyword)
       selected-result-options))
    (if (member :fully-qualified selected-result-options)
	(setf symbol-view +fully-qualified-symbol-view+)
	(setf symbol-view clim:+textual-view+)))
  (%maybe-update-result-display))

(defun %update-action-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (selected-action-option selected-values) clim:*application-frame*
    (setf selected-action-option 
	  (intern (string-upcase (clim:gadget-label selected-gadget)) :keyword))
    (if (eq selected-action-option :single)
	(progn
	  (setf selected-values (when selected-values (list (car selected-values))))
	  (clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'clear-action))
	  (clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'select-all-action)))
	(progn
	  (clim:activate-gadget (clim:find-pane-named clim:*application-frame* 'clear-action))
	  (clim:activate-gadget (clim:find-pane-named clim:*application-frame* 'select-all-action)))))
  (%maybe-update-output-display))

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
  (clim:stream-increment-cursor-position pane (+ x-offset 10) 5)
  (clim:surrounding-output-with-border
      (pane :shape :underline :ink clim:+black+)
    (clim:with-text-style (pane *apropos-navigator-heading-text-style*)
      (princ text pane))))

(defun %print-error (pane condition &optional (x-offset 0))
  (fresh-line pane)
  (clim:stream-increment-cursor-position pane (+ x-offset 10) 5)
  (clim:with-drawing-options (pane :ink clim:+red+)
    (%print-heading-text pane "Syntax Error")
    (fresh-line pane)
    (clim:stream-increment-cursor-position pane (+ x-offset 10) 0)
    (princ condition pane)))

(defun %print-text (pane text &optional (x-offset 0))
  (fresh-line pane)
  (clim:stream-increment-cursor-position pane (+ x-offset 10) 0)
  (princ text pane))

(defun take (n l)
  (subseq l 0 (if (< (length l) n) (if (> n (1- (length l))) (length l) (1- (length l))) n)))

(defun %render-output (frame pane)
  (declare (ignore frame))
  (with-slots (selected-values selected-output-option iapropos) clim:*application-frame*
    (flet ((print-symbol (sym type opt)
	     (ccase opt
	       (:selection
		(%print-heading-text pane (format nil "Selected symbols"))
		(let ((*print-escape* t))
		  (dolist (v selected-values)
		    (fresh-line pane)
		    (clim:stream-increment-cursor-position pane 10 0)
		    (clim:present v 'symbol :stream pane :view +fully-qualified-symbol-view+))))
	       (:object
		(%print-heading-text pane (format nil "Object (~A)" type))
		(fresh-line pane)
		(clim:stream-increment-cursor-position pane 10 0)
		(clim:present (symbol-object sym type) 'object
			      :stream pane :view clim:+textual-view+))
	       (:location
		(%print-heading-text pane (format nil "Location (~A)" type))
		(fresh-line pane)
		(clim:stream-increment-cursor-position pane 10 0)
		(clim:present (symbol-location sym type) 'source-location
			      :stream pane :view clim:+textual-view+))
	       (:documentation
		(%print-heading-text pane (format nil "Documentation (~A)" type))
		(%print-text pane (symbol-documentation (car selected-values) type)))
	       (:description
		(%print-heading-text pane (format nil "Description (~A)" type))
		(%print-text pane (symbol-description (car selected-values) type))))
	     ;;(princ #\Newline pane)
	     ;;(fresh-line pane)
	     (clim:stream-increment-cursor-position pane 0 5)
	     ))
      ;;(setf (clim:stream-cursor-position pane) (values 10 10))     
      (if (null selected-values)
	  (%print-heading-text pane (format nil "Empty selection"))
	  (if (eq selected-output-option :selection)
	      (print-symbol selected-values nil selected-output-option)
	      (dolist (sym selected-values)
		(let ((*print-escape* t))
		  (%print-heading-text pane (format nil "~S" sym)))
		(if (iapropos-bound-to iapropos) 
		    (print-symbol sym
				  (iapropos-bound-to iapropos) selected-output-option)
		    (dolist (type (list-symbol-bounding-types sym))
		      (print-symbol sym type selected-output-option)))))))))

(defun %render-symbol-result (frame pane)
  (with-slots (iapropos selected-values symbol-view)
      clim:*application-frame*
    (let* ((matching-symbols (iapropos-matching-symbols iapropos))
	   (symbols-to-print (take 400 matching-symbols)))
      (%print-heading-text pane
			   (format nil "Symbols (~A/~A~A)"
				   (length symbols-to-print)
				   (length matching-symbols)
				   (if (iapropos-result-overflow-p iapropos) "*" "")))
      (if (null matching-symbols)
	  (progn
	    (fresh-line pane)
	    (clim:stream-increment-cursor-position pane 5 0)
	    (princ "; no results" pane))
	  (dolist (sym symbols-to-print)
	    (fresh-line pane)
	    (clim:stream-increment-cursor-position pane 10 0)
	    (clim:with-drawing-options (pane :ink
					     (if (member sym selected-values)
						 clim:+blue+
						 clim:+black+))
	      (clim:present sym 'symbol :stream pane :view symbol-view)))))))

(defun %render-package-result (frame pane)
  (with-slots (iapropos)
      clim:*application-frame*
    (let* ((matching-packages (iapropos-matching-packages iapropos)))
      (%print-heading-text pane (format nil "Packages (~A)" (length matching-packages)))
      (if (null matching-packages)
	  (progn
	    (fresh-line pane)
	    (clim:stream-increment-cursor-position pane 5 0)
	    (princ "; no results" pane))
	  (dolist (package matching-packages)
	    (fresh-line pane)
	    (clim:stream-increment-cursor-position pane 5 0)
	    (clim:present package 'package :stream pane :view clim:+textual-view+))))))



;;;
;;; actions
;;;

(defun %update-selected-values ()
  (with-slots (selected-values selected-action-option iapropos) clim:*application-frame*
    (setf *return-values*
	  (ccase selected-action-option
	    (:single
	     (car selected-values))
	    (:multiple
	     (remove-duplicates selected-values))))))

(defun return-action (this-gadget)
  (declare (ignore this-gadget))
  (%update-selected-values)
  (clim:frame-exit clim:*application-frame*))

(defun copy-action (this-gadget)
  (declare (ignore this-gadget))
  (%update-selected-values)
  (with-input-from-string (input-stream (format nil "~S" *return-values*))
    (uiop:run-program "xclip -selection clipboard -i " :output nil :input input-stream)))

(defun kill-ring-action (this-gadget)
  (declare (ignore this-gadget))
  (%update-selected-values)
  (drei-kill-ring:kill-ring-standard-push drei-kill-ring:*kill-ring*
					  (format nil "~A" *return-values*)))

(defun clear-action (this-gadget)
  (declare (ignore this-gadget))
  (with-slots (selected-values) clim:*application-frame*
    (setf selected-values nil))
  (%maybe-update-output-display))

(defun select-all-action (this-gadget)
  (declare (ignore this-gadget))
  (with-slots (selected-values iapropos) clim:*application-frame*
    (setf selected-values (iapropos-matching-symbols iapropos)))
  (%maybe-update-output-display))


;;;
;;; commands
;;;

(define-apropos-navigator-command (com-quit-and-selected-values :menu t
						      :name t
						      :keystroke ((:meta #\q)))
    ()
  (with-slots (selected-values) clim:*application-frame*
    (setf *return-values* (remove-duplicates selected-values))
    (clim:frame-exit clim:*application-frame*)))

(define-apropos-navigator-command (com-select-symbol-for-return :name t)
    ((sym 'symbol :gesture :select))
  (with-slots (selected-values selected-action-option) clim:*application-frame*
    (if (eq selected-action-option :single)
	(setf selected-values (list sym))
	(if (member sym selected-values)
	    (setf selected-values (remove sym selected-values))
	    (setf selected-values (remove-duplicates (push sym selected-values))))))
  ;;(%maybe-update-result-display)
  (%maybe-update-output-display))

(define-apropos-navigator-command (com-select-package-for-return :name t)
    ((p 'package :gesture :select))
  (setf (clim:gadget-value
	 (clim:find-pane-named clim:*application-frame* 'package-regex-text-field))
	(format nil "^~A$" (package-name p))))

(define-apropos-navigator-command (com-select-object-for-return :name t)
    ((object 'object :gesture :select))
  (clouseau:inspector object))

(define-apropos-navigator-command (com-select-source-location :name t)
    ((object 'source-location :gesture :select))
  (climacs:edit-file (car object))
  (clim:execute-frame-command (climacs::find-climacs-frame)
			      (list 'drei-commands::com-goto-position (cdr object))))

  


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
        climi::selected-values nil)
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
