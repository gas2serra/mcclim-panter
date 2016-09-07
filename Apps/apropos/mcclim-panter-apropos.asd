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

(defsystem #:mcclim-panter-apropos
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim :climacs :clim-listener :cl-ppcre :anaphora :swank :closer-mop :clouseau)
  :components ((:file "mcclim-panter-apropos")
	       (:module "src"
			:serial t
			:depends-on ("mcclim-panter-apropos")
			:components
			((:file "utility")
			 (:file "iapropos")
			 (:file "iapropos-preselects")
			 (:file "presentations")))
	       (:module "gui"
			:serial t
			:depends-on ("src")
			:components
			((:file "parameters")
			 (:file "frame")
			 (:file "commands")
			 (:file "main"))))
  :description "Apropos Navigator")
