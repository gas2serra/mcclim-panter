(in-package :mcclim-panter-apropos)

;;;
;;; Preselect
;;;

(defun command-internals-symbol-p (symbol)
  "XXX, If this were implemented properly, it would scan for one of the
following strings: acceptor, partial or unparser. But since it is cheaper..
Perhaps hotpatch define-command to throw if one feeds it a command name with %?"
  (let* ((name (symbol-name symbol))
         (scanner (cl-ppcre:create-scanner "^COM-" :case-insensitive-mode t)))
    (and (cl-ppcre::scan scanner name) (cl-ppcre::scan #\% name))))
