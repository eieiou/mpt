;; -*- Lisp -*-

(defpackage #:chemboy-system
  (:use #:common-lisp #:asdf))

(in-package #:chemboy-system)

(defsystem chemboy
  :author "Peter Scott"
  :licence "LLGPL"
  :version "0.2"
  :components ((:file "unittest")
	       (:file "query-parsing")
	       (:file "conversions" :depends-on ("unittest"
						 "compound-names"
                                                 "compounds-table"
						 "query-parsing"))
;	       (:file "gui" :depends-on ("conversions"
;					 "rootsolve"))
	       (:file "compound-names")
	       (:file "compounds-table" :depends-on ("compound-names"))
	       (:file "rootsolve")
	       (:file "text-interface" :depends-on ("conversions"
						    "rootsolve")))
  :depends-on (periodic-table ;ltk
	       chemical-compounds))