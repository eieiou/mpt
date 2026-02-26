;; A text-based interface that should enable Chemboy to run almost anywhere.

(defpackage :chemboy-text
  (:use :common-lisp
	:chemboy-rootsolve)
  (:export #:chemboy-repl))

(in-package :chemboy-text)

(defun read-query ()
  "Prompt the user for a query and return the input"
  (format *query-io* "~&> ")			; Display prompt
  (finish-output *query-io*)
  ;; Read input. Should use readline where available.
  (read-line *query-io* nil ""))

(defparameter *repl-commands* '(("quit" .     :quit)
				("bye" .      :quit)
				("help" .     :help)
				("?" .        :help)
				("idealgas" . :ideal-gas)
				("combinedgas" . :combined-gas)
				("boyle" .    :boyle)
				("charles" .  :charles)
				("law" .      :law))
  "An alist mapping Chamboy REPL command strings to command symbols")

(defun get-chemboy-command (text)
  "Return a REPL command symbol if `text' is a command, or nil otherwise"
  (cdr (assoc text *repl-commands*
	      :test #'string=)))

(defparameter *help-string*
 "Welcome to Chemboy, a program to help with routine chemstry
calculations. While there is much more detailed help available in the
README file or online at <http://www.cliki.net/chemboy/>, here is a
simple overview of the things you can do with Chemboy.

To quit, type \"quit\" or \"bye\". To see this message again, type
\"help\" or just \"?\".

If you want to find out how many moles and liters are in 12 milligrams
of diatomic hydrogen, type \"12 mg H2\".

If you want to find out about the element Krypton, type either
\"Krypton\", \"krypton\", \"Kr\", or \"36\" (the atomic number of
Krypton).

If you want to find out the formula weight of methane, type in the
formula with spaces between elements, like this: \"C H4\". The same
goes for things like glucose, \"C6 H12 O6\".

To use laws like the Ideal gas law and the Combined gas law, type in
\"law\" and choose a law. There are short commands for the individual
laws: \"charles\", \"boyle\", \"idealgas\", and \"combinedgas\"."
 "The text that is printed when the help command is used")

(defun eval-query (query)
  "Evaluate a Chemboy query and return the text to be printed"
  (let ((command (get-chemboy-command query)))
    (if command
	(ecase command
	  (:quit (throw 'quit-repl nil))
	  (:help *help-string*)
	  (:law (select-law-dialog))
	  (:ideal-gas (ideal-gas-law-dialog))
	  (:combined-gas (combined-gas-law-dialog))
	  (:boyle (boyle-law-dialog))
	  (:charles (charles-law-dialog)))
      (or (conversions:parse-query query)
	  "Error: bad query. Have you read the manual?"))))

(defun print-query (text)
  "Print `text' formatted as a query result"
  (format *query-io* "~A~%~%" (or text "")))

(defun chemboy-repl ()
  "Start the Chemboy REPL"
  (format *query-io* "~&Welcome to Chemboy ~A. Type \"help\" for help.~%"
	  "0.3")			; FIXME: use global
  (catch 'quit-repl
    (loop (print-query (eval-query (read-query))))))


;; Chemical Laws
;;;;;;;;;;;;;;;;

(defun safe-read-number-from-string (string)
  "Read from string, safely, ensuring that the result is either
a number or nil"
  (let ((result (let ((*read-eval* nil))
                  (handler-case (read-from-string string)
                    (error () nil)))))
    (when (numberp result)
      result)))

(defun just-one-missing (&rest args)
  "Is just one of the arguments nil?"
  (= (count-if #'identity args)
     (1- (length args))))

(defun prompt-for (text)
  "Prompt the user for something described in `text'"
  (format *query-io* "~A: " text)
  (finish-output *query-io*)
  ;; FIXME: add readline support where available (CLISP?)
  (read-line *query-io* nil ""))

(defun display-message (text &rest arguments)
  "Display `text' formatted with `arguments' as a message"
  (format *query-io* "~&~A~%"
	  (apply #'format nil
		 text arguments))
  (finish-output *query-io*))

(defun boyle-law-dialog ()
  (format *query-io* "~&Enter all quantities except the one to solve for.
Use any units you like as long as they are consistant.~%~%")
  (let ((p1 (safe-read-number-from-string (prompt-for "Pressure 1")))
        (v1 (safe-read-number-from-string (prompt-for "Volume 1")))
        (p2 (safe-read-number-from-string (prompt-for "Pressure 2")))
        (v2 (safe-read-number-from-string (prompt-for "Volume 2"))))
    (if (not (just-one-missing p1 v1 p2 v2))
        (display-message "You're supposed to leave one thing blank!")
      (let ((result (coerce (rootsolve (boyle :p1 p1
					      :v1 v1
					      :p2 p2
					      :v2 v2))
			    'single-float)))
        (cond ((not p1)
               (display-message "Pressure 1: ~A" result))
              ((not v1)
               (display-message "Volume 1: ~A" result))
              ((not p2)
               (display-message "Pressure 2: ~A" result))
              ((not v2)
               (display-message "Volume 2: ~A" result)))))))

(defun charles-law-dialog ()
  (format *query-io* "~&Enter all quantities except the one to solve for.
Use any units you like for volume, but temperature
must be given in Kelvins.~%~%")
  (let ((t1 (safe-read-number-from-string (prompt-for "Temperature 1")))
        (v1 (safe-read-number-from-string (prompt-for "Volume 1")))
        (t2 (safe-read-number-from-string (prompt-for "Temperature 2")))
        (v2 (safe-read-number-from-string (prompt-for "Volume 2"))))
    (if (not (just-one-missing t1 v1 t2 v2))
        (display-message "You're supposed to leave one thing blank!")
      (let ((result (coerce (rootsolve (charles :t1 t1
						:v1 v1
						:t2 t2
						:v2 v2))
			    'single-float)))
        (cond ((not t1)
               (display-message "Temperature 1: ~A" result))
              ((not v1)
               (display-message "Volume 1: ~A" result))
              ((not t2)
               (display-message "Temperature 2: ~A" result))
              ((not v2)
               (display-message "Volume 2: ~A" result)))))))

(defun ideal-gas-law-dialog ()
  (format *query-io* "~&Enter all quantities except the one to solve for.
Temperature must be given in Kelvins; Pressure
must be given in kPa (kiloPascals); moles must, of
course, be given in moles; Volume must be given
in liters (or \"litres\" if you like British spellings).~%~%")
  (let ((p (safe-read-number-from-string (prompt-for "Pressure")))
        (v (safe-read-number-from-string (prompt-for "Volume")))
        (n (safe-read-number-from-string (prompt-for "Moles")))
        (temp (safe-read-number-from-string (prompt-for "Temperature"))))
    (if (not (just-one-missing p v n temp))
        (display-message "You're supposed to leave one thing blank!")
      (let ((result (coerce (rootsolve (ideal-gas :p p
						  :v v
						  :n n
						  :temp temp))
			    'single-float)))
        (cond ((not p)
               (display-message "Pressure: ~A" result))
              ((not v)
               (display-message "Volume: ~A" result))
              ((not n)
               (display-message "Moles: ~A" result))
              ((not temp)
               (display-message "Temperature: ~A" result)))))))

(defun combined-gas-law-dialog ()
  (format *query-io* "~&Enter all quantities except the one to solve for.
Temperature must be given in Kelvins; moles must, of
course, be given in moles; use any units you like for
everything else. If something (like temperature or
pressure) remains constant, put in 1 for it everywhere
to disregard it in the calculations.~%~%")
  (let ((t1 (safe-read-number-from-string (prompt-for "Temperature 1")))
        (v1 (safe-read-number-from-string (prompt-for "Volume 1")))
	(p1 (safe-read-number-from-string (prompt-for "Pressure 1")))
	(n1 (safe-read-number-from-string (prompt-for "Moles 1")))
        (t2 (safe-read-number-from-string (prompt-for "Temperature 2")))
        (v2 (safe-read-number-from-string (prompt-for "Volume 2")))
	(p2 (safe-read-number-from-string (prompt-for "Pressure 2")))
	(n2 (safe-read-number-from-string (prompt-for "Moles 2"))))
    (if (not (just-one-missing t1 v1 p1 n1 t2 v2 p2 n2))
        (display-message "You're supposed to leave one thing blank! If one
variable remains constant, just put 1 in for it everywhere.")
      (let ((result (coerce (rootsolve (combined-gas :t1 t1
						     :v1 v1
						     :p1 p1
						     :n1 n1
						     :t2 t2
						     :v2 v2
						     :p2 p2
						     :n2 n2))
			    'single-float)))
        (cond ((not t1)
               (display-message "Temperature 1: ~A" result))
              ((not v1)
               (display-message "Volume 1: ~A" result))
              ((not p1)
               (display-message "Pressure 1: ~A" result))
              ((not n1)
               (display-message "Moles 1: ~A" result))
	      ((not t2)
               (display-message "Temperature 2: ~A" result))
              ((not v2)
               (display-message "Volume 2: ~A" result))
              ((not p2)
               (display-message "Pressure 2: ~A" result))
              ((not n2)
               (display-message "Moles 2: ~A" result)))))))

(defun select-law-dialog ()
  "Ask user which law to use, then use it"
  (format *query-io* "~&Which chemistry law do you want to use?
Type in the number.~%~%")
  (format *query-io* "1. Boyle's Law (pressure & volume)~%")
  (format *query-io* "2. Charles' Law (volume & temp.)~%")
  (format *query-io* "3. Combined gas Law (everything)~%")
  (format *query-io* "4. Ideal gas Law (just one state)~%")
  (format *query-io* "~%> ")
  (finish-output *query-io*)
  (let ((selection (safe-read-number-from-string
		    (read-line *query-io* nil ""))))
    (case selection
      (1 (boyle-law-dialog))
      (2 (charles-law-dialog))
      (3 (combined-gas-law-dialog))
      (4 (ideal-gas-law-dialog))
      (t (progn (format *query-io* "~&Aborting...~%"))))))