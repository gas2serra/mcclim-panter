(in-package :mcclim-panter)

;;;
;;; Filters
;;;

(defun command-internals-symbol? (symbol)
  "XXX, If this were implemented properly, it would scan for one of the
following strings: acceptor, partial or unparser. But since it is cheaper..
Perhaps hotpatch define-command to throw if one feeds it a command name with %?"
  (let* ((name (symbol-name symbol))
         (scanner (cl-ppcre:create-scanner "COM-" :case-insensitive-mode t)))
    (and (cl-ppcre:scan scanner name) (cl-ppcre:scan #\% name))))

(defun external-symbol? (symbol)
  (declare (ignore symbol))
  t)

(defun bound? (symbol)
  (or (fboundp symbol) (boundp symbol)))

(defun unbound? (symbol)
  (not (bound? symbol)))

;;;
;;; Apropos functions
;;;
(defun package-apropos-list (search-regex)
  (let* ((out)
         (scanner (cl-ppcre:create-scanner search-regex :case-insensitive-mode t)))
    (dolist (p (list-all-packages))
      (when (cl-ppcre:scan scanner (package-name p))
        (push p out)))
    out))

(defun symbol-apropos-list (search-regex packages)
  (let ((swank::*buffer-package* (find-package :common-lisp-user))
	(swank::*buffer-readtable* *readtable*))
    (mapcar #'(lambda (s) (cons s
				(funcall
				 (swank::listify #'swank::briefly-describe-symbol-for-emacs) s)))
	    (sort (remove-duplicates
		   (cl-ppcre:regex-apropos-list search-regex packages))
		  #'swank::present-symbol-before-p))))
	 

;;;
;;; apropos
;;;  (swank:apropos-list-for-emacs "defun")
;;;
;;; apropos-symbols (string external-only case-sensitive package)
;;;

