(in-package :cl-user)

(defpackage mcclim-panter-apropos 
  (:use :cl :panter-apps)
  (:import-from :clim
		)
  (:export
   ;; utility functions
   #:symbol-external-p
   #:symbol-bound-to
   #:list-symbol-bounding-types
   #:symbol-documentation
   #:symbol-description
   ;; iapropos
   #:*default-iapropos-max-result-length*
   #:*symbol-bounding-types*
   #:iapropos
   #:iapropos-text
   #:iapropos-package-text
   #:iapropos-external-yes/no
   #:iapropos-documentation-yes/no
   #:iapropos-bound-to
   #:iapropos-subclass-of
   #:iapropos-metaclass-of
   #:iapropos-filter-fn
   #:iapropos-max-result-length
   #:iapropos-result-overflow-p
   #:iapropos-syntax-error-p
   #:iapropos-matching-packages
   #:iapropos-matching-symbols
   #:iapropos-matching-symbol-p
   ;; gui
   #:run-navigator
   ))

(in-package :mcclim-panter-apropos)
