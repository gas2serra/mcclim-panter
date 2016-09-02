(in-package :mcclim-panter-apropos)

;;;
;;; utility functions
;;;

(defun symbol-external-p (symbol)
  (swank::symbol-external-p symbol))

;;;
;;; parameters
;;;

(defparameter *default-iapropos-max-result-length* 1000)

;;;
;;; interactive apropos
;;;

(defclass iapropos ()
  ((apropos-text :initform nil
		 :accessor iapropos-text)
   (cached-apropos-scanner :initform nil)
   (package-apropos-text :initform ""
			 :accessor iapropos-package-text)
   (external-only-p :type '(member t nil)
		    :initform nil
		    :accessor iapropos-external-only-p)
   (bounded-to :type '(member nil
		       :variable :function :generic-function
		       :class :macro
		       :setf :type)
	       :initform nil
	       :accessor iapropos-bounded-to)
   (subclass-of :initform nil
		:accessor iapropos-subclass-of)
   (metaclass-of :initform nil
		 :accessor iapropos-metaclass-of)
   (filter-fn :initform nil
	     :accessor iapropos-filter-fn)
   (max-result-length :initform *default-iapropos-max-result-length*
		      :accessor iapropos-max-result-length)
   (result-overflow :initform nil
		    :reader iapropos-result-overflow)
   (cached-matching-packages :initform (list-all-packages))
   (cached-matching-symbols :initform nil)
   (syntax-error-p :initform t
		   :accessor iapropos-syntax-error-p)))

;;; generic funtions
(defgeneric iapropos-matching-symbols (iapropos))
(defgeneric iapropos-matching-packages (iapropos))
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

(defmethod (setf iapropos-external-only-p) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-bounded-to) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-external-only-p) :after (val (iapropos iapropos))
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
    (if (not (and package-apropos-text (string/= package-apropos-text "")))
	(setf cached-matching-packages (list-all-packages))
	(setf cached-matching-packages 
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
	       syntax-error
	       result-overflow) iapropos
    (setf cached-matching-symbols
	  (let ((swank::*buffer-package* (find-package :common-lisp-user))
		(swank::*buffer-readtable* *readtable*)
		(out)
		(i 0))
	    (setf result-overflow nil)
	    (block iter
	      (with-package-iterator (next cached-matching-packages :external :internal)
		(loop (multiple-value-bind (morep symbol) (next)
			(when (not morep)
			  (return-from iter))
			(when (= i max-result-length)
			  (setf out (remove-duplicates out))
			  (setf i (length out))
			  (when (= i max-result-length)
			    (setf result-overflow t)
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
  (with-slots (cached-apropos-scanner external-only-p bounded-to
	       subclass-of metaclass-of filter-fn) iapropos
    (and
     (if external-only-p
	 (symbol-external-p symbol)
	 t)
     (if bounded-to
	 (ccase bounded-to
	   (:variable
	    (boundp symbol))
	   (:function
	    (fboundp symbol))
	   (:macro
	    (macro-function symbol))
	   (:class
	    (and (find-class symbol nil)
		 (if subclass-of
		     (subtypep symbol subclass-of)
		     t)
		 (if metaclass-of
		     (subtypep (type-of (find-class symbol)) metaclass-of)
		     t)))
	   (:generic-function
	    (and (fboundp symbol)
		 (typep (symbol-function symbol) 'generic-function)))
	   ((:setf :type)
	    (not (eq (getf (swank/backend::describe-symbol-for-emacs symbol) bounded-to 'cl:t)
		     t))))
	 t)
     (if filter-fn
	 (funcall filter-fn symbol)
	 t)
     (if cached-apropos-scanner
	 (cl-ppcre:scan cached-apropos-scanner (symbol-name symbol))
	 t))))

;;;
;;; TODO
;;;

#|

has-documentation-yes/no
:yes :no nil

external-yes/no
 :yes :no nil
|#
