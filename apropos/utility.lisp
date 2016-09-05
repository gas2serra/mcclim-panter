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
  (let ((types (remove-if #'(lambda (type)
			      (not (symbol-bound-to symbol type)))
			  *symbol-bounding-types*)))
    (cond
      ((member :generic-function types)
       (remove :function types))
      (t
       types))))
	
(defun symbol-documentation (symbol type)
  (let ((doc (getf (swank/backend::describe-symbol-for-emacs symbol)
		   (case type
		     (:class
		      :type)
		     (:generic-function
		      #+sbcl :generic-function
		      #+ccl :function)
		     (:macro
		      #+sbcl :macro
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
    (:setf
     #+sbcl (swank/sbcl::setf-expander symbol)
     #+ccl nil)
    (:type
     nil
     )))

(defun symbol-location (symbol type)
  (let ((definitions (swank::find-definitions symbol)))
    #+sbcl
    (ccase type
      (:variable
       (cdr (find-if #'(lambda (x) (eq 'defvar (caar x))) definitions)))
      (:function
       (cdr (find-if #'(lambda (x) (eq 'defun (caar x))) definitions)))
      (:generic-function
       (cdr (find-if #'(lambda (x) (eq 'defgeneric (caar x))) definitions)))
      (:macro
       (cdr (find-if #'(lambda (x) (eq 'defmacro (caar x))) definitions)))
      (:class
       (cdr (find-if #'(lambda (x) (eq 'defclass (caar x))) definitions)))
      (:setf
       (cdr (find-if #'(lambda (x) (eq 'define-setf-expander (caar x))) definitions)))
      (:type
       (cdr (find-if #'(lambda (x) (eq 'defclass (caar x))) definitions))))
    #+ccl
    (ccase type
      (:variable
       (cdr (find-if #'(lambda (x) (eq 'variable (caar x))) definitions)))
      (:function
       (cdr (find-if #'(lambda (x) (eq 'defun (caar x))) definitions)))
      (:generic-function
       (cdr (find-if #'(lambda (x) (eq 'defgeneric (caar x))) definitions)))
      (:macro
       (cdr (find-if #'(lambda (x) (eq 'defmacro (caar x))) definitions)))
      (:class
       (cdr (find-if #'(lambda (x) (eq 'defclass (caar x))) definitions)))
      (:setf
       (cdr (find-if #'(lambda (x) (eq 'define-setf-expander (caar x))) definitions)))
      (:type
       (cdr (find-if #'(lambda (x) (eq 'defclass (caar x))) definitions))))))
      
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

;;;
;;; utility
;;;

(defun command-internals-symbol-p (symbol)
  "XXX, If this were implemented properly, it would scan for one of the
following strings: acceptor, partial or unparser. But since it is cheaper..
Perhaps hotpatch define-command to throw if one feeds it a command name with %?"
  (let* ((name (symbol-name symbol))
         (scanner (cl-ppcre:create-scanner "^COM-" :case-insensitive-mode t)))
    (and (cl-ppcre::scan scanner name) (cl-ppcre::scan #\% name))))


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
