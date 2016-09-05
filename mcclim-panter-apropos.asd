#|
  This file is a part of mcclim-panter project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
 Apropos Navigator

  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-panter-apropos-asd
  (:use :cl :asdf))
(in-package #:mcclim-panter-apropos-asd)

(defsystem mcclim-panter-apropos
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim :clim-listener :cl-panter :cl-ppcre :anaphora :swank :closer-mop)
  :components (
	       (:module "apropos"
			:serial t
			:components
			((:file "mcclim-panter-apropos")
			 (:file "utility")
			 (:file "iapropos")
			 (:file "presentations")
			 (:file "navigator-parameters")
			 (:file "navigator"))))
  :description "Apropos Navigator")


