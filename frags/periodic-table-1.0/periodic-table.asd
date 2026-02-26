;; -*- Lisp -*-

(defpackage #:periodic-table-system
  (:use #:common-lisp #:asdf))

(in-package #:periodic-table-system)

(defsystem periodic-table
  :author "Peter Scott"
  :licence "LLGPL"
  :version "1.0"
  :components ((:file "unittest")
	       (:file "elements" :depends-on ("unittest"))
	       (:file "periodic-table" :depends-on ("elements"))))