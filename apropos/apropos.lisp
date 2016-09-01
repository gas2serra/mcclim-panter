(in-package :mcclim-panter-apropos)

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

#|
(defun bound? (symbol)
  (or (fboundp symbol) (boundp symbol)))

(defun unbound? (symbol)
  (not (bound? symbol)))
|#

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

(defun symbol-apropos-list (search-regex packages &optional (max-result-length 100))
  (let ((swank::*buffer-package* (find-package :common-lisp-user))
	(swank::*buffer-readtable* *readtable*))
    (let ((out)
	  (scanner (cl-ppcre:create-scanner search-regex :case-insensitive-mode t))
	  (i 0))
      (block iter
	(with-package-iterator (next packages :external :internal)
	  (loop (multiple-value-bind (morep symbol) (next)
		  (cond ((not morep)
			 (return-from iter))
			((= i max-result-length)
			 (return-from iter))
			((cl-ppcre:scan scanner (symbol-name symbol))
			 (push symbol out)
			 (incf i)))))))
      (sort
       out
       #'swank::present-symbol-before-p))))
	 

;;;
;;; apropos
;;;  (swank:apropos-list-for-emacs "defun")
;;;
;;; apropos-symbols (string external-only case-sensitive package)
;;;

#|
(defun apropos-symbols (string external-only case-sensitive package)
  (let ((packages (or package (remove (find-package :keyword)
                                      (list-all-packages))))
        (matcher  (make-apropos-matcher string case-sensitive))
        (result))
    (with-package-iterator (next packages :external :internal)
      (loop (multiple-value-bind (morep symbol) (next)
              (cond ((not morep) (return))
                    ((and (if external-only (symbol-external-p symbol) t)
                          (funcall matcher symbol))
                     (push symbol result))))))
    result))
|#

