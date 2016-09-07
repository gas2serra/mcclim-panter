#|
  This file is a part of mcclim-panter project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
 Task Manager

  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-panter-task-manager-asd
  (:use :cl :asdf))
(in-package #:mcclim-panter-task-manager-asd)

(defsystem #:mcclim-panter-task-manager
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim)
  :components ((:file "mcclim-panter-task-manager")
	       (:module "src"
			:serial t
			:depends-on ("mcclim-panter-task-manager")
			:components
			((:file "presentations")))
  	       (:module "gui"
			:depends-on ("src")
			:serial t
			:components
			((:file "parameters")
			 (:file "frame")
			 (:file "commands")
			 (:file "main"))))
  :description "Task Manager")
