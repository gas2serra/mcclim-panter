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

(defsystem #:mcclim-panter-internals
    :components ((:file "packages")))

(defsystem #:mcclim-panter
  :version "0.2"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim-panter-internals
	       :mcclim-panter-sys
	       :mcclim-panter-core)
  :serial t
  :components ((:file "initialize"))
  :description "McClim Developer Suite")
