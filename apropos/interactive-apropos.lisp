(in-package :mcclim-panter-apropos)

;;;
;;; utility functions
;;;

(defun symbol-external-p (symbol)
  (swank::symbol-external-p symbol))

;;;
;;; interactive apropos
;;;

(defclass iapropos ()
  ((apropos-text :initform nil
		 :accessor iapropos-text)
   (apropos-scanner :initform nil)
   (package-apropos-text :initform ""
			 :accessor iapropos-package-text)
   (external-only-p :initform nil
		    :accessor iapropos-external-only-p)
   (kind :type (member nil :variable :function :class :generic-function)
	 :initform nil
	 :accessor iapropos-kind)
   (max-result-length :initform 100
		      :accessor iapropos-max-result-length)
   (result-overflow :initform nil
		    :reader iapropos-result-overflow)
   (cached-matching-packages :initform (list-all-packages))
   (cached-matching-symbols :initform nil)
   (syntax-error :initform nil
		 :reader iapropos-syntax-error)))

(defmethod (setf iapropos-package-text) :after (text (iapropos iapropos))
  (declare (ignore text))
  (with-slots (syntax-error) iapropos
    (setf syntax-error nil)
    (%update-matching-packages iapropos)))

(defmethod (setf iapropos-text) :after (text (iapropos iapropos))
  (declare (ignore text))
  (with-slots (syntax-error apropos-text apropos-scanner) iapropos
    (setf syntax-error nil)
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf syntax-error condition)
			(return-from iapropos-text condition))))
      (setf apropos-scanner (cl-ppcre:create-scanner apropos-text :case-insensitive-mode t))
      (%update-matching-symbols iapropos))))

(defmethod (setf iapropos-external-only-p) :after (val (iapropos iapropos))
  (declare (ignore val))
  (with-slots (syntax-error) iapropos
    (setf syntax-error nil)
    (%update-matching-symbols iapropos)))

(defmethod (setf iapropos-kind) :after (val (iapropos iapropos))
  (declare (ignore val))
  (with-slots (syntax-error) iapropos
    (setf syntax-error nil)
    (%update-matching-symbols iapropos)))

(defmethod (setf iapropos-external-only-p) :after (val (iapropos iapropos))
  (declare (ignore val))
  (with-slots (syntax-error) iapropos
    (setf syntax-error nil)
    (%update-matching-symbols iapropos)))

(defgeneric iapropos-matching-packages (iapropos))

(defmethod iapropos-matching-packages ((iapropos iapropos))
  (with-slots (cached-matching-packages syntax-error external-only-p) iapropos
    (if syntax-error
	nil
	(let ((packages cached-matching-packages))
	  packages))))

(defgeneric iapropos-matching-symbols (iapropos))

(defmethod iapropos-matching-symbols ((iapropos iapropos))
  (with-slots (cached-matching-symbols syntax-error external-only-p kind) iapropos
    (if syntax-error
	nil
	(let ((symbols cached-matching-symbols))
	  symbols))))

;;;
;;; updating
;;; 

(defun iapropos-check-symbol (iapropos symbol)
  (with-slots (apropos-scanner external-only-p kind) iapropos
    (and
     (if external-only-p
	 (symbol-external-p symbol)
	 t)
     (ccase kind
       (:variable
	(boundp symbol))
       (:function
	(fboundp symbol))
       (:class
	(find-class symbol nil))
       (:generic-function
	(and (fboundp symbol)
	     (typep (symbol-function symbol) 'generic-function)))
       ((nil)
	t))
     (cl-ppcre:scan apropos-scanner (symbol-name symbol)))))

(defun %update-matching-packages (iapropos)
  (with-slots (cached-matching-packages
	       cached-matching-symbols
	       package-apropos-text
	       syntax-error) iapropos
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf syntax-error condition)
			(setf cached-matching-packages nil)
			(setf cached-matching-symbols nil)
			(return-from %update-matching-packages condition))))
      (if (and package-apropos-text (string/= package-apropos-text ""))
	  (setf cached-matching-packages (package-apropos-list package-apropos-text))
	  (setf cached-matching-packages (list-all-packages))))
    (%update-matching-symbols iapropos)))

(defun %update-matching-symbols (iapropos)
  (with-slots (cached-matching-packages
	       cached-matching-symbols
	       apropos-text
	       max-result-length
	       syntax-error
	       result-overflow) iapropos
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf syntax-error condition)
			(setf cached-matching-packages nil)
			(setf cached-matching-symbols nil)
			(return-from %update-matching-symbols condition))))
      (if apropos-text
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
			      (when (iapropos-check-symbol iapropos symbol)			      
				(push symbol out)
				(incf i))))))
		  (sort
		   (remove-duplicates out)
		   #'swank::present-symbol-before-p)))
	  (setf cached-matching-symbols nil)))))


