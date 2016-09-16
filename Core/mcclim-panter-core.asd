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

(defsystem mcclim-panter-core
  :version "0.2"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:asdf)
  :components ((:module "src"
			:serial t
			:components
			((:file "debugger")
			 (:file "initialize"))))
  :description "McClim Developer Suite")
