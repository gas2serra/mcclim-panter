#|
  This file is a part of mcclim-panter project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
  Debugger

  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-panter-debugger-asd
  (:use :cl :asdf))
(in-package #:mcclim-panter-debugger-asd)

(defsystem mcclim-panter-debugger
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim :clim-listener :cl-panter :anaphora :clouseau)
  :components ((:module "debugger"
			:serial t
			:components
			((:file "mcclim-panter-debugger")
			 (:file "debugger"))))
  :description "Debugger")


