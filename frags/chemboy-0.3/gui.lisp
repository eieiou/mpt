(defpackage :chemboy-gui
  (:use :common-lisp
	:ltk
	:conversions)
  (:export #:run-chemboy-gui))

(in-package :chemboy-gui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Debugging could be handy, but most of the time it's just annoying
  (setf *debug-tk* nil))

(defclass chemboy-gui ()
  ((frame :accessor frame)
   (output-textbox :accessor output-textbox)
   (input-grid :accessor input-grid)
   (query-input :accessor query-input)
   (query-button :accessor query-button)))

(defmethod calculate-query ((gui chemboy-gui))
  (let* ((query (text (query-input gui)))
	 (result (parse-query query))
	 (textbox (textbox (output-textbox gui))))
    (append-text textbox
	  (format nil "~&> ~A~%~A~%~%"
		  query
		  (or result
		      "Error: bad query. Have you read the manual?")))
    (see textbox
	 26)))

(defmethod initialize-instance :after ((gui chemboy-gui) &key)
  (setf (frame gui)
	(make-instance 'frame))
  (setf (output-textbox gui)
	(make-instance 'scrolled-text
		       :master (frame gui)))
  (setf (input-grid gui)
	(make-instance 'frame
		       :master (frame gui)))
  (setf (query-input gui)
	(make-instance 'entry
		       :master (input-grid gui)
		       :width 80))
  (setf (query-button gui)
	 (make-instance 'button
			:master (input-grid gui)
			:text "Calculate"
			:default "active"
			:command (lambda ()
				   (calculate-query gui))))
  (wm-title *tk* "Chemboy"))

(defmethod run ((gui chemboy-gui))
  (pack (frame gui))
  (pack (output-textbox gui)
	:side :bottom
	:expand t
	:fill 'both)
  (pack (input-grid gui)
	:expand t
	:fill 'both)
  (grid (query-input gui)
	0 0)
  (grid (query-button gui)
	0 1))

(defun run-chemboy-gui ()
  (with-ltk
   (run (make-instance 'chemboy-gui))))

;(chemboy-gui:run-chemboy-gui)