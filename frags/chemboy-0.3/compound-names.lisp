(defpackage :compound-names
  (:use :common-lisp
	:compounds)
  (:export #:compound-notes
	   #:compound-notes-name
	   #:compound-notes-description
	   #:defcompound
	   #:get-compound-notes
	   #:get-compound-description-text))

(in-package :compound-names)

;; Compounds often have certain names that we want to point out.
;; Na Cl, for instance, is "table salt". Our scheme here is just
;; going to be checking to see if two compounds have the same
;; formula weights, and if they do then they are considered equal.

(defstruct compound-notes
  name description)

(defparameter *compounds-table* (make-hash-table)
  "A table mapping formula weights to compound notes")

(defun defcompound (compound name &optional description)
  "Define a compound, given as either a compound designator or a string in
compound syntax, with a name and an optional description"
  (setf (gethash (formula-weight (get-compound compound)) *compounds-table*)
	(make-compound-notes :name name
			     :description description)))

(defun get-compound-notes (compound)
  "Get compound notes, if any, for a compound given as a compound designator
or a string in compound syntax"
  (gethash (formula-weight (get-compound compound)) *compounds-table*))

(defun get-compound-description-text (compound)
  "Get a textual description of `compound', enclosed in parens with a
leading space"
  (let ((notes (get-compound-notes compound)))
    (if notes
	(let ((name (compound-notes-name notes))
	      (description (compound-notes-description notes)))
	  (format nil " (~A)"
		  (if description
		      (format nil "~A, ~A" name description)
		    name)))
      "")))