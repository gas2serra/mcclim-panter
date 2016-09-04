(in-package :mcclim-panter-apropos)

;;;
;;; Constants
;;;

(defparameter *symbol-bounding-types* '(:variable :function :generic-function
				       :class :macro :setf :type))

;;;
;;; utility functions
;;;

(defun symbol-external-p (symbol)
  "Return t only if the symbol is external"
  (swank::symbol-external-p symbol))

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

(defun symbol-object (symbol type)
  (ccase type
    (:variable
     (symbol-value symbol))
    (:function
     (symbol-function symbol))
    (:macro
     (macro-function symbol))
    (:class
     (find-class symbol))
    (:generic-function
     (symbol-function symbol))
    ((:setf :type)
     nil
     )))

(defun symbol-location (symbol type)
  (let ((definitions (swank::find-definitions symbol)))
    (ccase type
      (:variable
       (symbol-value symbol))
      (:function
       (cdr (find-if #'(lambda (x) (eq 'defun (caar x))) (swank::find-definitions 'cl:cons))))
      (:macro
       (macro-function symbol))
      (:class
       (find-class symbol))
      (:generic-function
       (symbol-function symbol))
      (:setf
       nil)
      (:type
       (car (find-if #'(lambda (x) (eq 'deftype (caar x))) (swank::find-definitions 'cl:cons))))
       )))
      


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
;;; TODO
;;;


;;; source location
#|
(swank/backend:find-source-location (find-class 'mcclim-panter-apropos::iapropos))
(:LOCATION
 (:FILE "/home/gas/Projects/CL/my/mcclim-panter/apropos/iapropos.lisp")
 (:POSITION 186)
 (:SNIPPET "(defclass iapropos ()
  ((apropos-text :initform nil
		 :accessor iapropos-text)
   (cached-apropos-scanner :initform nil)
   (package-apropos-text :initform \"\"
			 :accessor iapropos-package-text)
   (external-yes/no :type '(member nil :yes :no)
		    :in"))

(swank/backend:find-source-location (symbol-function 'mcclim-panter-apropos::symbol-documentation))
(:LOCATION
 (:FILE "/home/gas/Projects/CL/my/mcclim-panter/apropos/utility.lisp")
 (:POSITION 904)
 (:SNIPPET "(defun symbol-documentation (symbol type)
  (let ((doc (getf (swank/backend::describe-symbol-for-emacs symbol)
		   (case type
		     (:class
		      :type)
		     (:generic-function
		      #+sbcl :generic-function
		      #+ccl :function)
		     (otherwi"))
|#
