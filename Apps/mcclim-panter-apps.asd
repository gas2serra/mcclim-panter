#|
  This file is a part of mcclim-panter project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-panter-asd
  (:use :cl :asdf))
(in-package #:mcclim-panter-asd)

(defsystem mcclim-panter-apps
  :version "0.2"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim-panter
	       :mcclim-panter-apropos
	       :mcclim-panter-debugger
	       :mcclim-panter-task-manager)
  :serial t
  :components (
	       (:file "initialize"))
  :description "")
