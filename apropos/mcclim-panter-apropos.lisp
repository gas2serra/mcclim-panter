(in-package :cl-user)

(defpackage mcclim-panter-apropos 
  (:use :cl)
  (:import-from :clim
		)
  (:export
   ;; iapropos
   #:*defoult-iapropos-max-result-length*
   #:iapropos
   #:iapropos-text
   #:iapropos-package-text
   #:iapropos-external-only-p
   #:iapropos-bounded-to
   #:iapropos-subclass-of
   #:iapropos-metaclass-of
   #:iapropos-max-result-lengt
   #:iapropos-result-overflow
   #:iapropos-syntax-error-p
   #:iapropos-matching-packages
   #:iapropos-matching-symbols
   #:iapropos-matching-symbol-p
   ;; gui
   #:run-navigator
   ))

(in-package :mcclim-panter-apropos)
