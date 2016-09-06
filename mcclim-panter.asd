#|
  This file is a part of mcclim-panter project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
  McClim Developer Suite

  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-panter-asd
  (:use :cl :asdf))
(in-package #:mcclim-panter-asd)

(defsystem #:mcclim-panter
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:clim-listener
	       :drei-mcclim
	       :climacs
	       :mcclim-panter-apropos
	       :mcclim-panter-debugger
	       :mcclim-panter-task-manager)
  :components ((:module "src"
			:serial t
			:components
			((:file "mcclim-panter")
			 (:file "drei-extension")
			 (:file "listener-extension"))))
  :description "McClim Developer Suite")
