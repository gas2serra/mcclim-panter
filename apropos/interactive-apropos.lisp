(in-package :mcclim-panter-apropos)

;;;
;;; utility functions
;;;

(defun symbol-external-p (symbol)
  "Return t only if the symbol is external"
  (swank::symbol-external-p symbol))

(defparameter *symbol-bounding-types* '(:variable :function :generic-function
					:class :macro :setf :type))

(defun symbol-bound-to (symbol type)
  (ccase type
    (:variable
     (boundp symbol))
    (:function
     (fboundp symbol))
    (:macro
     (macro-function symbol))
    (:class
     (find-class symbol nil))
    (:generic-function
     (and (fboundp symbol)
	  (typep (symbol-function symbol) 'generic-function)))
    ((:setf :type)
     (not (eq (getf (swank/backend::describe-symbol-for-emacs symbol) type 'cl:t)
	      t)))))

(defun list-symbol-bounding-types (symbol)
  (remove-if #'(lambda (type)
		 (not (symbol-bound-to symbol type)))
	     *symbol-bounding-types*))

(defun symbol-documentation (symbol type)
  (let ((doc (getf (swank/backend::describe-symbol-for-emacs symbol)
		   (case type
		     (:class
		      :type)
		     (:generic-function
		      #+sbcl :generic-function
		      #+ccl :function)
		     (otherwise
		      type))
		   'cl:t)))
    (if (member doc '(t nil :NOT-DOCUMENTED))
	nil
	doc)))

(defun symbol-description (symbol type)
  (with-output-to-string (*standard-output*)
    (case type
      ((:variable nil)
       (describe symbol))
      (:function
       (describe (symbol-function symbol)))
      (:macro
       (describe (macro-function symbol)))
      (:class
       (describe (find-class symbol)))
      (:generic-function
       (describe (symbol-function symbol)))
      (:setf
       #+sbcl (describe (sb-int:info :setf :expander symbol))
       #+ccl (describe (ccl:setf-function-spec-name `(setf ,symbol))))
      (:type
       #+sbcl (describe (sb-kernel:values-specifier-type symbol))
       #+ccl (describe (or (find-class symbol nil) symbol))))))

;;;
;;; parameters
;;;

(defparameter *default-iapropos-max-result-length* 1000
  "The max length of the result")

;;;
;;; interactive apropos
;;;

(defclass iapropos ()
  ((apropos-text :initform nil
		 :accessor iapropos-text)
   (cached-apropos-scanner :initform nil)
   (package-apropos-text :initform ""
			 :accessor iapropos-package-text)
   (external-yes/no :type '(member nil :yes :no)
		    :initform nil
		    :accessor iapropos-external-yes/no)
   (documentation-yes/no :type '(member nil :yes :no)
			 :initform nil
			 :accessor iapropos-documentation-yes/no)
   (bound-to :type '(member nil
		       :variable :function :generic-function
		       :class :macro
		       :setf :type)
	       :initform nil
	       :accessor iapropos-bound-to)
   (subclass-of :initform nil
		:accessor iapropos-subclass-of)
   (metaclass-of :initform nil
		 :accessor iapropos-metaclass-of)
   (filter-fn :initform nil
	      :accessor iapropos-filter-fn)
   (max-result-length :initform *default-iapropos-max-result-length*
		      :accessor iapropos-max-result-length)
   (result-overflow-p :initform nil
		      :reader iapropos-result-overflow-p)
   (cached-matching-packages :initform (list-all-packages))
   (cached-matching-symbols :initform nil)
   (syntax-error-p :initform t
		   :accessor iapropos-syntax-error-p))
  (:documentation "Interactive apropos class based on cl-ppcre and swank"))

;;; generic funtions
(defgeneric iapropos-matching-symbols (iapropos)
  (:documentation "Return the list of symbols that match the specified criteria"))
(defgeneric iapropos-matching-packages (iapropos)
  (:documentation "Return the list of packages that match the specified criteria"))
(defgeneric iapropos-matching-symbol-p (iapropos symbol))

;;; methods
(defmethod (setf iapropos-text) :after (text (iapropos iapropos))
  (declare (ignore text))
  (with-slots (syntax-error-p apropos-text cached-apropos-scanner cached-matching-symbols) iapropos
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf cached-matching-symbols nil)
			(unless syntax-error-p
			  (format *debug-io* "iapropos: ~A~%" condition)
			  (return-from iapropos-text condition)))))
      (when apropos-text
	(setf cached-apropos-scanner (cl-ppcre:create-scanner apropos-text :case-insensitive-mode t)))
      (%iapropos-update-matching-symbols iapropos))))

(defmethod (setf iapropos-package-text) :after (text (iapropos iapropos))
  (declare (ignore text))
  (with-slots (syntax-error-p cached-matching-symbols cached-matching-packages) iapropos
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf cached-matching-packages nil)
			(setf cached-matching-symbols nil)
			(unless syntax-error-p
			  (format *debug-io* "iapropos: ~A~%" condition)
			  (return-from iapropos-package-text condition)))))
      (%iapropos-update-matching-packages iapropos))))

(defmethod (setf iapropos-external-yes/no) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-bound-to) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-documentation-yes/no) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-subclass-of) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-metaclass-of) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-filter-fn) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod iapropos-matching-packages ((iapropos iapropos))
  (with-slots (cached-matching-packages) iapropos
    cached-matching-packages))

(defmethod iapropos-matching-symbols ((iapropos iapropos))
  (with-slots (cached-matching-symbols) iapropos
    cached-matching-symbols))

(defmethod iapropos-matching-symbol-p ((iapropos iapropos) symbol)
  (%iapropos-matching-symbol-p iapropos symbol))

;;;
;;; private generic functions
;;;

(defgeneric %iapropos-update-matching-symbols (iapropos))
(defgeneric %iapropos-update-matching-packages (iapropos))

(defmethod %iapropos-update-matching-packages ((iapropos iapropos))
  (with-slots (cached-matching-packages package-apropos-text) iapropos
    (setf cached-matching-packages
	  (if (not (and package-apropos-text (string/= package-apropos-text "")))
	      (list-all-packages)
	      (let ((scanner (cl-ppcre:create-scanner package-apropos-text :case-insensitive-mode t))
		    (out))
		(dolist (p (list-all-packages))
		  (when (cl-ppcre:scan scanner (package-name p))
		    (push p out)))
		out))))
  (%iapropos-update-matching-symbols iapropos))

(defmethod %iapropos-update-matching-symbols ((iapropos iapropos))
  (with-slots (cached-matching-packages
	       cached-matching-symbols
	       apropos-text
	       max-result-length
	       result-overflow-p) iapropos
    (setf cached-matching-symbols
	  (let ((swank::*buffer-package* (find-package :common-lisp-user))
		(swank::*buffer-readtable* *readtable*)
		(out)
		(i 0))
	    (setf result-overflow-p nil)
	    (block iter
	      (with-package-iterator (next cached-matching-packages :external :internal)
		(loop (multiple-value-bind (morep symbol) (next)
			(when (not morep)
			  (return-from iter))
			(when (= i max-result-length)
			  (setf out (remove-duplicates out))
			  (setf i (length out))
			  (when (= i max-result-length)
			    (setf result-overflow-p t)
			    (return-from iter)))
			(when (%iapropos-matching-symbol-p iapropos symbol)			      
			  (push symbol out)
			  (incf i))))))
	    (sort
	     (remove-duplicates out)
	     #'swank::present-symbol-before-p)))))

;;;
;;; private functions
;;;

(defun %iapropos-matching-symbol-p (iapropos symbol)
  (with-slots (cached-apropos-scanner external-yes/no documentation-yes/no bound-to
	       subclass-of metaclass-of filter-fn) iapropos
    (and
     (if external-yes/no
	 (eq (not (symbol-external-p symbol))
	     (eq external-yes/no :no))
	 t)
     (if bound-to
	 (symbol-bound-to symbol bound-to)
	 t)
     (if (eq bound-to :class)
	 (and
	  (if subclass-of
	      (subtypep symbol subclass-of)
	      t)
	  (if metaclass-of
	      (subtypep (type-of (find-class symbol)) metaclass-of)
	      t))
	 t)
     (if filter-fn
	 (funcall filter-fn symbol)
	 t)
     (if (and bound-to documentation-yes/no)
	 (eq (not (symbol-documentation symbol bound-to))
	     (eq documentation-yes/no :no))
	 t)
     (if cached-apropos-scanner
	 (cl-ppcre:scan cached-apropos-scanner (symbol-name symbol))
	 t))))
