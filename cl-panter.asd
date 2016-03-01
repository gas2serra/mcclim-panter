#|
  This file is a part of mcclim-panter project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
  Debugging utility

  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:cl-panter-asd
  (:use :cl :asdf))
(in-package #:cl-panter-asd)

(defsystem cl-panter
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on ()
  :components ((:module "src"
		:serial t	
                :components
                ((:file "cl-panter"))))
  :description "Common lisp debugging utilities")


