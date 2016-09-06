(in-package :mcclim-panter-apropos)

;;;
;;; application frame
;;;

(clim:define-application-frame apropos-navigator ()
  ((selected-values :initform nil)
   (selected-result-options :initform '(:fully-qualified))
   (selected-output-option :initform ':selection)
   (selected-action-option :initform ':single)
   (symbol-view :initform +fully-qualified-symbol-view+)
   (iapropos :initform (make-instance 'iapropos)))
  (:command-table (apropos-navigator
		   :inherit-from (edit-menu)
		   :menu (("Edit" :menu edit-menu))))
  (:menu-bar t)
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
		  :activate-callback #'(lambda (gadget)
					 (declare (ingore gadget))
					 (com-quit))
		  :label "return")
   (copy-action :push-button
		:activate-callback #'(lambda (gadget)
				       (declare (ingore gadget))
				       (com-edit-copy-to-clipboard))
		:label "copy")
   (kill-ring-action :push-button 
		     :activate-callback #'(lambda (gadget)
					    (declare (ingore gadget))
					    (com-edit-copy-to-kill-ring))
		     :label "kill-ring"))
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
			    kill-ring-action))))
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
;;; frame initialization
;;;

(defmethod clim::note-frame-enabled ((fm clim:frame-manager) (frame apropos-navigator))
  (setf (clim:command-enabled 'com-edit-select-all clim:*application-frame*) nil)
  (setf (clim:command-enabled 'com-edit-select-none clim:*application-frame*) nil))

;;;
;;; input/output
;;;

(defmethod clim:frame-standard-input ((frame apropos-navigator))
  (car (clim:sheet-children
	(clim:find-pane-named clim:*application-frame* 'symbol-regex-text-field))))


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
      (setf (iapropos-symbol-text iapropos) value)
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
	  (setf (clim:command-enabled 'com-edit-select-all clim:*application-frame*) nil)
	  (setf (clim:command-enabled 'com-edit-select-none clim:*application-frame*) nil))
	(progn
	  (setf (clim:command-enabled 'com-edit-select-all clim:*application-frame*) t)
	  (setf (clim:command-enabled 'com-edit-select-none clim:*application-frame*) t))))
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
		    (clim:present v 'symbol :stream pane :view +fully-qualified-symbol-view+)
		    (clim:stream-increment-cursor-position pane 10 0)
		    (anaphora:awhen (list-symbol-bounding-types v)
		      (princ anaphora:it pane)))))
	       (:object
		(%print-heading-text pane (format nil "Object (~A)" type))
		(let ((obj (symbol-object sym type)))
		  (when obj
		    (fresh-line pane)
		    (clim:stream-increment-cursor-position pane 10 0)
		    (clim:present obj 'object
				  :stream pane :view clim:+textual-view+))))
	       (:location
		(%print-heading-text pane (format nil "Location (~A)" type))
		(let ((loc (symbol-location sym type)))
		  (when loc
		    (fresh-line pane)
		    (clim:stream-increment-cursor-position pane 10 0)
		    (clim:present loc 'source-location
				  :stream pane :view clim:+textual-view+))))
	       (:documentation
		(%print-heading-text pane (format nil "Documentation (~A)" type))
		(let ((doc (symbol-documentation (car selected-values) type)))
		  (when doc
		    (%print-text pane doc))))
	       (:description
		(%print-heading-text pane (format nil "Description (~A)" type))
		(let ((des (symbol-description (car selected-values) type)))
		  (when des
		    (%print-text pane des)))))
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
;;; return values
;;;

(defvar *return-values* nil)

(defun %update-return-values ()
  (with-slots (selected-values selected-action-option iapropos) clim:*application-frame*
    (setf *return-values*
	  (ccase selected-action-option
	    (:single
	     (car selected-values))
	    (:multiple
	     (remove-duplicates selected-values))))))

;;;
;;; run
;;;

(defun run-apropos-navigator ()
  (let ((*return-values* nil))
    (let* ((frame (clim:make-application-frame 'apropos-navigator)))
      (setf (clim:frame-current-layout frame) :default)
      (clim:run-frame-top-level frame :name "apropos-navigator"))
    *return-values*))

;;;
;;;
;;;

(defmacro with-fixed-vertical-scroll-bar (pane &body body)
  (let ((vscrollbar (gensym "VSCROLLBAR"))
	(vsb-value (gensym "VSB-VALUE")))
    `(let* ((,vscrollbar (slot-value (clim:sheet-parent
				     (clim:sheet-parent
				     ,pane))
				    'climi::vscrollbar))
	    (,vsb-value (slot-value ,vscrollbar 'climi::value)))
       ,@body
       (progn
	 (climi::drag-callback ,vscrollbar nil nil ,vsb-value)
	 (setf (clim:gadget-value ,vscrollbar) ,vsb-value)))))
