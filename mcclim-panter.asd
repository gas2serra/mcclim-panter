#|
  This file is a part of mcclim-panter project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
  Debugging utility

  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-panter-asd
  (:use :cl :asdf))
(in-package #:mcclim-panter-asd)

(defsystem mcclim-panter
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim :clim-listener :cl-panter)
  :components ((:module "mcclim"
		:serial t	
                :components
                ((:file "mcclim-panter")
		 (:file "commands")
		 (:file "frame")
		 (:file "main"))))
  :description "Debugging utility")


