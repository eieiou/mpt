;; Taken from SAPA. The copyright notice from the original file reads:
;; SAPA, Version 1.0; Copyright 1993, Donald B. Percival, All Rights Reserved
;; Use and copying of this software and preparation of derivative works
;; based upon this software are permitted.  Any distribution of this
;; software or derivative works must comply with all applicable United
;; States export control laws.
;; 
;; This software is made available AS IS, and no warranty -- about the
;; software, its performance, or its conformity to any
;; specification -- is given or implied.

(defpackage :chemboy-rootsolve
  (:use :common-lisp)
  (:export boyle
	   charles
	   ideal-gas
	   combined-gas
	   secant-method
	   rootsolve))

(in-package :chemboy-rootsolve)

;; Sadly, Corman Lisp 2.5 does not have epsilon constants defined.
#+cormanlisp (defconstant doubte-float-epsilon 1.1102230246251568d-16)

(defun secant-method
       (f
        x-left
        x-right
        &key
        (accuracy (* 10.0d0 double-float-epsilon))
        (maximum-number-of-iterations 50))
  "given
   [1] f (required)
       ==> a function with a single argument
   [2] x-left (required)
       ==> left-hand bracket for the desired root;
           i.e., left-hand bracket <= desired root
   [3] x-right (required)
       ==> right-hand bracket for the desired root;
           i.e., desired root <= right-hand bracket
   [4] accuracy (keyword; (* 10.0 single-float-epsilon))
       ==> desired relative accuracy for computed rood
   [5] maximum-number-of-iterations (keyword; 50)
       ==> maximum number of iterations
returns
   [1] a root of f in [x-left, x-right];
       i.e., a value x in that interval
       such that f(x) = 0
   [2] the number of iterations required
---
Note: this function is based loosely on rtsec,
Section 9.2, Numerical Recipes, Second Edition"
  (let ((f-left (funcall f x-left))
        (f-right (funcall f x-right))
        x-mid f-mid approx-f-mid approx-f-prime-mid delta-x x-new f-new
        denom-for-accuracy-test)
    (dotimes (j maximum-number-of-iterations
		(values x-new maximum-number-of-iterations))
      (setf x-mid (* 0.5d0 (+ x-left x-right))
            f-mid (funcall f x-mid)
            approx-f-mid (* 0.5d0 (+ f-left f-right))
            approx-f-prime-mid (/ (- f-right f-left)
                                  (- x-right x-left))
            delta-x (/ approx-f-mid  approx-f-prime-mid)
            x-new (- x-mid delta-x)
            f-new (funcall f x-new))
      (setf denom-for-accuracy-test (+ (abs x-mid) (abs x-new)))
      (if (or (zerop f-new)
	      (< (/ (abs delta-x) denom-for-accuracy-test) accuracy))
        (return (values x-new (1+ j))))
      (if (>= (* f-mid f-left) 0d0)
        (setf x-left x-mid
              f-left f-mid))
      (if (>= (* f-mid f-right) 0d0)
        (setf x-right x-mid
              f-right f-mid))
      (if (>= (* f-new f-left) 0d0)
        (setf x-left x-new
              f-left f-new)
        (setf x-right x-new
              f-right f-new)))))


;(defun exp-2 (x)
;  (- (exp x) 2.0d0))
;
;(secant-method
; #'exp-2 0.0 20.0)


;; End SAPA code

(defmacro getvar (name &key no-zero)
  "Get either the value of a variable or the value of the symbol X. If
`no-zero' is true, then this will not return zero."
  `(or ,name
      (if (and ,no-zero
	       (= x 0.0d0))
	  0.000001d0
	x)))

;; Boyle's Law
;;;;;;;;;;;;;;

(defun boyle (&key p1 v1 p2 v2)
  "Boyle's law put into a form suitable for root solving. Return a
function which takes one argument and returns a number."
  #'(lambda (x)
      (the double-float
	(- (* (getvar p1)
	      (getvar v1))
	   (* (getvar p2)
	      (getvar v2))))))

;; Charles' Law
;;;;;;;;;;;;;;;

(defun charles (&key t1 v1 t2 v2)
  "Charles' law put into a form suitable for root solving. Return a
function which takes one argument and returns a number."
  #'(lambda (x)
      (the double-float
	(- (* (getvar v1)
	      (getvar t2))
	   (* (getvar v2)
	      (getvar t1))))))

;; Ideal Gas Law
;;;;;;;;;;;;;;;;

(defconstant +molar-gas-constant+ 8.314472d0
  "The molar gas constant in L * kPa / mole * K. This works with moles,
Kelvins, kPa, and L.")

(defun ideal-gas (&key p v n temp (r +molar-gas-constant+))
  "Ideal gas law put into a form suitable for root solving. Return a
function which takes one argument and returns a number. Note that the
units are moles, Kelvins, kPa, and L unless you specify a different R"
  #'(lambda (x)
      (the double-float
	(- (* (getvar n)
	      r
	      (getvar temp))
	   (* (getvar p)
	      (getvar v))))))

(defun combined-gas (&key p1 v1 p2 v2 n1 t1 n2 t2)
  "Combined gas law put into a form suitable for root solving. Return a
function which takes one argument and returns a number."
  #'(lambda (x)
      (the double-float
	(- (* (getvar p1)
	      (getvar v1)
	      (getvar n2)
	      (getvar t2))
	   (* (getvar n1)
	      (getvar t1)
	      (getvar p2)
	      (getvar v2))))))

(defun rootsolve (f)
  "Find a root of f between -10000001d0 and 10000000d0 using the secant
method and returning the answer and number of iterations"
  (secant-method
   f
   -10000001d0 10000000d0))

;; p1 = 270 Pa
;; v1 = 12 L
;; v2 = 15 L
;; p2 = ???  ; 216 Pa
;(rootsolve
; (boyle :p1 270d0
;        :v1 12d0
;        :v2 15d0))                      ; 216 Pa
;
;(rootsolve (charles :v1 25
;                    :t1 295
;                    :t2 273))           ; ~23.1
;
;(rootsolve (combined-gas :v1 25
;                         :t1 295
;                         :t2 273
;                         :p1 1
;                         :p2 1
;                         :n1 1
;                         :n2 1))        ; Same as above