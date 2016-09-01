(in-package :mcclim-panter-apropos)

;;; interactive apropos
(defclass iapropos ()
  ((apropos-text :initform nil
		 :reader iapropos-text)
   (package-apropos-text :initform nil
			 :reader iapropos-package-text)
   (external-only-p :initform nil
		    :accessor iapropos-external-only-p)
   (kind :type (member nil :variable :function :class)
	 :initform nil
	 :accessor iapropos-kind)
   (cached-matching-packages :initform nil)
   (cached-matching-symbols :initform nil)
   (syntax-error :initform nil
		 :reader iapropos-syntax-error)))


(defmethod (setf iapropos-package-text) (text (iapropos iapropos))
  (with-slots (package-apropos-text syntax-error) iapropos
    (setf package-apropos-text text)
    (setf syntax-error nil)
    (%update-matching-packages iapropos)))

(defmethod (setf iapropos-text) (text (iapropos iapropos))
  (with-slots (apropos-text syntax-error) iapropos
    (setf apropos-text text)
    (setf syntax-error nil)
    (%update-matching-symbols iapropos)))

(defgeneric iapropos-matching-packages (iapropos))

(defmethod iapropos-matching-packages ((iapropos iapropos))
  (with-slots (cached-matching-packages syntax-error external-only-p) iapropos
    (if syntax-error
	syntax-error
	(let ((packages cached-matching-packages))
	  packages))))

(defgeneric iapropos-matching-symbols (iapropos))

(defmethod iapropos-matching-symbols ((iapropos iapropos))
  (with-slots (cached-matching-symbols syntax-error external-only-p kind) iapropos
    (if syntax-error
	syntax-error
	(let ((symbols cached-matching-symbols))
	  (when external-only-p
	    (setf symbols (remove-if-not #'symbol-external-p symbols)))
	  (ccase kind
	    (:variable
	     (setf symbols (remove-if-not #'boundp symbols)))
	    (:function
	     (setf symbols (remove-if-not #'fboundp symbols)))
	    (:class
	     (setf symbols (remove-if-not #'(lambda (s)
					      (find-class s nil))
					  symbols)))
	    ((nil)
	     ))
	  symbols))))

;;;
;;; updating
;;; 

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
      (if package-apropos-text
	  (setf cached-matching-packages (package-apropos-list package-apropos-text))
	  (setf cached-matching-packages nil)))
    (%update-matching-symbols iapropos)))

(defun %update-matching-symbols (iapropos)
  (with-slots (cached-matching-packages
	       cached-matching-symbols
	       apropos-text
	       syntax-error) iapropos
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf syntax-error condition)
			(setf cached-matching-packages nil)
			(setf cached-matching-symbols nil)
			(return-from %update-matching-symbols condition))))
      (if (and apropos-text (> (length apropos-text) 3))
	  (setf cached-matching-symbols
		(symbol-apropos-list apropos-text cached-matching-packages))
	  (setf cached-matching-symbols
		nil)))))
